# Praise the Lord!

require 'deduckt/version'
require 'json'
require 'parser/ruby25'
require 'ast'
require 'set'

# TODO: This file needs some refactoring
# still it's a good test of rubocop now :)
# it also needs to be split but for now it's easier to filter for it in tracing
# currently it uses the Tracepoint API and basically collects most relevant 
# :call, :c_call, :b_call, :return, :c_return, :b_return events
# :line are too many and don't seem so important, :raise are a little bit tricky to analyze
# :thread_ are still not useful to us
# we need to optimize this code too: i feel it can run a lot faster
module Tools
  module_function


  SIMPLE_TYPES = {}

  class Value
    attr_accessor :kind, :label, :children, :typ, :args, :code, :line, :column, :text, :i, :f, :return_type, :methods, :fields, :docstring, :declaration

    def initialize(kind:, label: nil, children: nil, typ: nil, is_iterator: nil, is_class: nil, args: nil, code: nil, line: -1, column: -1, text: nil, i: nil, f: nil, val: nil, return_type: nil, methods: nil, fields: nil, docstring: nil, declaration: nil)
      @kind = kind
      @label = label
      @children = children
      @typ = typ
      @is_iterator = is_iterator
      @is_class = is_class
      @args = args
      @code = code
      @line = line
      @column = column
      @text = text
      @i = i
      @f = f
      @val = val
      @return_type = return_type
      @methods = methods
      @fields = fields
      @docstring = docstring
      @declaration = declaration
    end

    def to_json(depth = 0)
      # i prefer this : simpler to map to Nim and optimize than @kwargs meta
      labels = [:kind, :label, :children, :typ, :isIterator, :isClass, :args, :code, :line, :column, :text, :i, :f, :val, :returnType, :methods, :fields, :docstring, :declaration]
      values = [@kind, @label, @children, @typ, @is_iterator, @is_class, @args, @code, @line, @column, @text, @i, @f, @val, @return_type, @methods, @fields, @docstring, @declaration]
      Hash[labels.zip(values).reject { |l, v| v.nil? }.map { |l, v| [l, v] }].to_json(depth)
    end

    def [](c)
      if @children
        @children[c]
      end
    end

    def []=(c, value)
      @children ||= []
      @children[c] = value
    end

    def length
      @children ? @children.length : 0
    end

  end
  def val(kind, **kwargs)
    Value.new(kind: kind, **kwargs)
  end
  p val(:Variable).to_json

  def simple(name)
    SIMPLE_TYPES[name] ||= {kind: :Simple, label: name}
    SIMPLE_TYPES[name]
  end

  def operator(name)
    val(:Operator, label: name)
  end

  def variable(name)
    val(:Variable, label: name)
  end

  def int(i)
    val(:Int, i: i)
  end

  def float(f)
    val(:Float, f: f)
  end

  def bool(b)
    val(:Bool, val: b)
  end

  def string(text)
    val(:String, text: text)
  end

  def symbol(text)
    val(:Symbol, text: text)
  end
end

module Deduckt
  extend Tools

  TABLE_TYPE = {kind: :Generic, label: 'Table', genericArgs: ['K', 'V']}
  # object types
  INTER_CLASS = 0
  INTER_METHOD = 1
  INTER_CALL = 2
  INTER_VARIABLE = 3

  PATTERN_STDLIB = {puts: -> a { {kind: INTER_CALL, children: [{kind: INTER_VARIABLE, label: 'echo'}] + a , typ: {kind: :Nil} } } }

  # map them to interlang
  KINDS = {
    lvasgn: :Assign, array: :Sequence, hash: :NimTable, 
    begin: :Code, dstr: :Docstring, return: :Return,
    yield: :Yield, next: :Continue, break: :Break,
    False: :Bool, True: :Bool, while: :While,
    when: :Of, erange: :Range, zsuper: :Super, kwbegin: :Try,
    rescue: :Except, resbody: :Code, optarg: :Optarg}

  OPERATORS = Set.new [:+, :-, :*, :/, :==, :>, :<, :>=, :<=, :"!=", :"&&", :"||", :"!"]
  NORMAL = Set.new [:RubyIf, :RubyPair, :RubyCase, :RubySelf, :RubyOptarg]
  VOID_TYPE = {kind: :Simple, label: 'Void'}
  NIL_VALUE = val(:Nil)


  class TraceRun
    extend Tools
    include Tools

    def initialize(program, args, options)
      @program = program
      @args = args
      @module_patterns = options[:module_patterns] # matching modules with patterns
      @outdir = options[:outdir] # output directory for json
      @trace = []
      @inter_traces = {}
      @inter_types = {}
      @stack = [] # information for self
      @call_lines = []
      @simple_types = {}
      @processing = {} # processing types without looping too much in the same type
    end

    # each call invokes that so make it not slow
    # adding directly trace information
    def trace_calls(data)
      @call_lines.push(data.lineno)
      method_lines = @inter_traces[data.path][:method_lines]
      if method_lines.key?(data.lineno)
        m = method_lines[data.lineno]
        if m.kind == :NodeMethod && data.event != :b_call ||
           m.kind == :Block && data.event == :b_call
          @stack.push('')
          m.args.each do |arg|
            if arg.label == :self
              arg.typ = load_type(data.binding.receiver)
              if not arg.typ.nil?
                @stack[-1] = arg.typ[:label]
              end
            else
              # only if type isn't already recorded: maybe we have union it later but for now this
              if arg.typ.nil? 
                begin
                  arg.typ = load_type(data.binding.local_variable_get(arg.label))
                rescue
                  arg.typ = VOID_TYPE
                end
              end
            end
          end
        end
      end
    end

    def load_hash_type(arg)
      if arg.length == 0
        key = VOID_TYPE
        value = key
      else
        arg.each do |k, v|
          key = load_type(k)
          value = load_type(v)
          break
        end
      end
      {kind: :Compound, args: [key, value], original: TABLE_TYPE}
    end

    def load_type(arg)
      if arg.class.nil?
        return VOID_TYPE
      end

      klass = arg.class
      variables = []
      if klass == Integer
        return simple(:Int)
      elsif klass == Float
        return simple(:Float)
      elsif klass == NilClass
        return VOID_TYPE
      elsif klass == String
        return simple(:String)
      elsif klass == TrueClass || klass == FalseClass
        return simple(:Bool)
      elsif klass == Hash
        return load_hash_type(arg)
      elsif klass == Symbol
        return simple(:Symbol)
      elsif klass == Array
        return {kind: :Compound, original: simple(:Sequence), args: [load_type(arg[0])]}

      else
        if !@processing.key?(klass.name)
          @processing[klass.name] = []
          variables = arg.instance_variables.map do |a|
            load_type(arg.instance_variable_get(a)).tap { |b| b[:fieldLabel] = a.to_s[1 .. -1] if b }
          end
          @processing[klass.name] = variables
        else    
          return simple(klass.name.to_sym)
        end
      end
      
      
      if klass.is_a?(Class) && variables.length > 0
        # ok for now
        if !@inter_types[klass.name.to_sym]
          label = klass.name.to_sym
          res = {kind: :Object, label: label}

          if @outdir != 'test'
            res[:fields] = [] # TODO inheritance variables
          else
            res[:fields] = variables
          end
          @inter_types[label] = res
          if label.to_s.include?('::')
            # p label.to_s.split('::')[-1].to_sym
            @inter_types[label.to_s.split('::')[-1].to_sym] = res
          end
          res = simple(label) # Praise the Lord!
        end
        # far simpler to just move all the classes to %types: easier to process and search
      end
      res
    end

    def load_method(args, return_type)
      val(:NodeMethod, args: args.map { |arg| load_type(arg) }, return_type: load_type(return_type))
    end

    def union(left, right)
      case left[:kind]
      when :Method
        same = left[:args] == right[:args] && left[:returnType] == right[:returnType]
        if same
          [left, true]
        else
          [{kind: :MethodOverload, overloads: [left, right]}, false]
        end
      when :MethodOverload
        nil #p left
      when :Simple
        if left[:label] == right[:label]
          [left, true]
        else
          [{kind: :Union, variants: [left, right]}, true]
        end
      end
    end

    class InterTranslator
      include Tools

      def initialize(ast, comments, input)
        @ast = ast
        @comments = comments
        @comments = Hash[@comments.map { |comment| [comment.loc.line, comment.text] }]
        @raw = [''] + input.split("\n")
      end

      def process
        $t.disable # TODO same event
        $t2.disable
        res = {imports: [], main: [], classes: [], lines: {}, method_lines: {}}
        @inter_ast = res
        children = [@ast]
        local = @ast
        if @ast.type == :begin
          children = @ast.children
          local = children[-1]
        end
        # HACK: improve

        if local.type == :module
          while local.children.length >= 2 && local.children[1].type == :module
            local = local.children[1]
          end
          children = local.children[1 .. -1]
        end
        children.each do |it|
          if it.type == :class
            @current_class = it.children[0].children[1].to_s
            res[:classes].push(process_node it)
            # puts "CLASS"
            # p res[:classes][-1]
            new_class = res[:classes][-1].children.select { |it| it.kind != :RubyClass }
            classes = res[:classes][-1].children.select { |it| it.kind == :RubyClass }
            if classes.length > 0
              res[:classes][-1].children = new_class
              res[:classes] += classes
            end

            find_docstring(res[:classes][-1])
            @current_class = ''
          elsif it.type == :send && it.children[0].nil? && it.children[1] == :require
            res[:imports].push(it.children[2].children[0])
          else
            # p it, process_node(it)
            res[:main].push(process_node it)
          end
        end
        $t.enable
        $t2.enable
        res
      end

      def find_docstring(object)
        i = object.line - 1
        while i > 2
          if @raw[i].nil? || !@raw[i].lstrip.start_with?('#')
            break
          end
          if @comments.key?(i)
            if object.docstring.nil?
              object.docstring = []
            end
            object.docstring.push(@comments[i].gsub(/\A#+/, ''))

          end
          i -= 1
        end
        object.docstring.reverse! if !object.docstring.nil? 

      end

      def get_kind(type)
        if KINDS.key?(type)
          KINDS[type]
        else
          :"Ruby#{type.capitalize}"
        end
      end

      def process_node(node)
        if node.class == Parser::AST::Node
          # p node
          if respond_to?(:"process_#{node.type}")
            return send :"process_#{node.type}", node
          end
          begin
            line = node.loc.line
            column = node.loc.column
          rescue
            line = -1
            column = -1
          end
          if node.type == :yield
            @is_iterator = true
          end
          if node.type == :def || node.type == :defs
            index = {def: 0, defs: 1}[node.type]
            @is_iterator = false
            value = val(
              :NodeMethod,
              label: variable(node.children[index]),
              args: [variable(:self)] + node.children[index + 1].children.map { |it| process_node it },
              code: node.children[index + 2 .. -1].map { |it| process_node it },
              line: line,
              column: column,
              is_iterator: @is_iterator,
              is_class: node.type == :defs)
            find_docstring(value)
            value.docstring = value.docstring || []
            @inter_ast[:method_lines][line] = value
            return value
          elsif node.type == :block
            value = val(
              :Block,
              label: variable(''),
              args: node.children[1].children.map { |it| process_node it },
              code: node.children[2 .. -1].map { |it| process_node it })
            @inter_ast[:method_lines][line] = value
            child = process_node node.children[0]
            child.children.push(value)
            child.line = line
            child.column = column
            return child
          end
          value = val(get_kind(node.type), children: node.children.map { |it| process_node it })
          if node.type == :send
            if value[0].kind == :RubyConst && (value[1].kind == :Variable && value[1].label == :new || value[1].kind == :Symbol && value[1].text == :new)
              value = val(:New, children: [variable(value[0].label)] + value.children[2 .. -1])
            end
            if value.length > 1
              value[1].kind = :Variable
              value[1].label = value[1].text
              value[1].text = nil
            end
            if !@inter_ast[:lines].key?(line)
              @inter_ast[:lines][line] = []
            end
            @inter_ast[:lines][line].push(value)
            value
          else
            if node.type == :if && node.children[1].nil?
              value[0] = val(
                :UnaryOp,
                children: [operator(:not), value[0]],
                typ: simple(:Bool)
              )
              value.children[1] = value[2]
              value.children.pop
            end
            value
          end.tap { |t| if !node.nil?; t.line = line; t.column = column; end }
        elsif node.class == Integer
          int(node)
        elsif node.class == Float
          float(node)
        elsif node.class == String
          string(node)
        elsif node.class == Symbol
          symbol(node)
        elsif node.nil?
          NIL_VALUE
        end
      end

      def process_const(node)
        val(:RubyConst, label: node.children[1])
      end

      def process_op_asgn(node)
        left = process_node(node.children[0].children[0])
        op = val(:Operator, label: node.children[1])

        right = process_node(node.children[2])
        if left.kind == :Symbol
          left.kind = :Variable
          left.label = left.text
          left.text = nil
        end
        val(:AugOp, children: [left, op, right])
      end

      def process_or_asgn(node)
        left = process_node(node.children[0].children[0])
        op = val(:Operator, label: :or)
        right = process_node(node.children[1])
        if left.kind == :Symbol
          left.kind = :Variable
          left.label = left.text
          left.text = nil
        end
        val(:Assign, children: [left, val(:BinOp, children: [left, op, right])])
      end

      def process_int(node)
        int(node.children[0])
      end

      def process_str(node)
        string(node.children[0])
      end

      def process_dstr(node)
        val(:Docstring, text: node.children.map { |it| it.children[0] }.join)
      end

      def process_float(node)
        float(node.children[0])
      end

      def process_sym(node)
        symbol(node.children[0])
      end

      def process_true(node)
        val(:Bool, val: true)
      end

      def process_false(node)
        val(:Bool, val: false)
      end

      def process_and(node)
        val(
          :BinOp, 
          children: [
            operator(:and),
            process_node(node.children[0]),
            process_node(node.children[1])])
      end

      def process_or(node)
        val(
          :BinOp, 
          children: [
            operator(:and),
            process_node(node.children[0]),
            process_node(node.children[1])])
      end

      def process_ivar(node)
        val(:Attribute, children: [val(:Self), string(node.children[0][1 .. -1])])
      end

      def process_lvar(node)
        variable(node.children[0])
      end

      def process_gvar(node)
        variable(node.children[0])
      end

      def process_ivasgn(node)
        val(
          :Assign,
          children: [
            val(:Attribute,
              children: [
                val(:Self),
                string(text: node.children[0][1 .. -1])]),
            process_node(node.children[1])])
      end

      def process_lvasgn(node)
        val(:Assign, children: [variable(node.children[0]), process_node(node.children[1])])
      end

      def process_block_pass(node)
        arg = node.children[0].children[0].nil? ? '' : node.children[0].children[0][0 .. -1]
        
        # proc(it: typ): returnType { it.arg }
        val(
          :Block,
          args: [variable(:it)],
          code: [
            val(
              :Attribute,
              children: [variable(:it), string(arg)])])
      end

      def process_casgn(node)
        value = node.children[-1]
        if value.type == :send && value.children[-1] == :freeze
          value = value.children[0]
        end
        # how to do it when global and several have the same name? label = "#{@current_class}#{node.children[1]}"
        label = node.children[1]
        val(
          :Assign,
          children: [variable(label), process_node(value)],
          declaration: :Const)
      end

      def process_nil(node)
        NIL_VALUE
      end

      def process_arg(node)
        variable(node.children[0])
      end

      def process_module(node)
        process_node(node.children[1])
      end

      def process_masgn(node)
        if node.children[0].children.length == 1
          right = val(
            :Index,
            children: [
              node.children[1].type == :send ? process_node(node.children[1]) : process_node(node.children[1].children[0].children[0]),
              int(0)])
          val(
            :Assign,
            children: [
              variable(node.children[0].children[0].children[-1]),
              right])
        else
          NIL_VALUE
        end
      end
    end


    def generate_ast(path)
      input = File.read(path)
      ast, comments = Parser::Ruby25.parse_with_comments(input)
      InterTranslator.new(ast, comments, input).process
    end


    def compile_call(child)
      arg_index = 1
      m =if !child.typ.nil? || !child[2].nil?
        val(:Call, children: child.children[1 .. -1].map { |l| compile_child l }, typ: child.typ)
      else
        arg_index = -1
        child.children[1]
      end
      [m, arg_index]
    end

    def compile_send(child)
      m = if !child.typ.nil? || !child[2].nil?
        arg_index = 2
        val(:Send, children: child.children.map { |l| compile_child l }, typ: child.typ)
      else
        arg_index = -1
        val(:Attribute, children: child.children.map { |l| compile_child l }, typ: child.typ)
      end
      if m[1].kind == :Variable
        m[1] = string(m[1].label)
      end
      if [:Send, :Attribute].include?(m.kind) && OPERATORS.include?(m[1].text.to_sym)
        arg_index = -1
        op = operator(m[1].text)
        if !m[2].nil?
          left, right = m[0], m[2]
          m = val(:BinOp, children: [op, left, right], typ: m.typ)
        else
          value = m[0]
          m = val(:UnaryOp, children: [op, value], typ: m.typ)
        end
      elsif m.kind == :Send && m[1].text.to_sym == :[]
        left, right = m[0], m[2]
        m = val(:Index, children: [left, right], typ: m.typ)
      end
      [m, arg_index]
    end

    def compile_invoke(child, top_level = false)
      if child[0].kind == :Nil || child[0].kind == :Self && top_level
        compile_call(child)    
      else
        compile_send(child)
      end
    end
      
    def compile_kwargs(m, arg_index)
      new_args = []
      m.children[arg_index .. -1].each do |arg|
        if arg.kind == :NimTable
          new_args += arg.children
        else
          new_args.push(arg)
        end
      end

      m.children = m.children[0 .. arg_index - 1] + new_args
      m
    end

    def compile_child child, top_level = false
      if child.nil?
        return NIL_VALUE
      end
      if child.kind == :RubySend
        m, arg_index = compile_invoke(child, top_level = top_level)
        if arg_index != -1
          m = compile_kwargs(m, arg_index)
        end
        m.line = child.line
        m.column = child.column
        m
      elsif !child.children.nil?
        res = child
        res.children = child.children.map { |it| compile_child it }
        if NORMAL.include?(res.kind)
          res.kind = res.kind.to_s[4 .. -1].to_sym
        end
        res
      elsif !child.code.nil?
        res = child
        res.code = child.code.map { |it| compile_child it }
        res
      else
        # p child
        child
      end
    end

    def compile traces
      $types_no_definition = Hash[@inter_types.map { |k, v| [k, v] }]
      traces.each do |path, file|
        file[:main] = file[:main].map do |child|
          compile_child(child, top_level = true)
        end

        file[:classes] = file[:classes].map do |klass|
          parent = klass[1]
          label = klass[0].label
          if parent.nil? || parent.kind == :Nil
            parent = VOID_TYPE
          elsif parent.kind == :RubyConst
            parent = simple(parent.label)
          end
          mercy = klass[2]
          if mercy.kind == :RubyBegin
            mercy = mercy.children
          else
            mercy = klass.children[2 .. -1]
          end
          if mercy.length == 1 && mercy[0].kind == :Code
            children = mercy[0].children
            mercy = mercy[0].children.select { |n| n.kind == :NodeMethod }
            b = children.reject { |n| n.kind == :NodeMethod }
            file[:main] += b.map { |it| compile_child(it) }
          end

          if @inter_types.key?(label)
            @inter_types[label][:base] = parent
          end
          $types_no_definition.delete label

          val(
            :Class,
           label: label,
           methods: mercy.map { |met| {label: met.label.label, node: compile_child(met)} },
           fields: [],
           docstring: klass.docstring || [],
           typ: @inter_types[label])
        end
        
      end
      @inter_traces['%types'] = $types_no_definition
    end

    def generate
      compile @inter_traces
      #p @outdir
      #p @inter_traces.length
      File.write(File.join(@outdir, 'lang_traces.json'), JSON.pretty_generate(@inter_traces))
    end

    def module_of_interest?(path)
      if path.include?('mixin')
        return true
      end
      @module_patterns.each do |pattern|
        if pattern == 'rubocop'
          return false if !path.include?(pattern)
        else
          return false if !path.include?('/' + pattern)
        end
      end
      true
    end

    def execute(load = true)
      trace_run = self

      $t = TracePoint.new(:call, :c_call, :b_call) do |tp|
        next if tp.method_id == :method_added || !trace_run.module_of_interest?(tp.path)

        # TODO require require_relative

        if ![:new, :initialize, :enable, :disable, :require_relative, :require].include?(tp.method_id) #tp.path != "deduckt.rb" &&
          if !@inter_traces.key?(tp.path)
            @inter_traces[tp.path] = generate_ast(tp.path)
          end
          trace_calls(tp)
        end
      end

      $t2 = TracePoint.new(:return, :c_return, :b_return) do |tp|
        next if tp.method_id == :method_added || !trace_run.module_of_interest?(tp.path)

        if ![:new, :initialize, :enable, :disable].include?(tp.method_id)
          from_path, from_line, *_ = caller[1].split(':')
          from_line = from_line.to_i
          path = tp.path
          line = tp.lineno
          typ = load_type(tp.return_value)
          send_in = false
          send_position = [path, line]

          if @inter_traces.key?(from_path)
            if @inter_traces[from_path][:lines].key?(from_line)
              send_in = true
              send_position = [from_path, from_line]
            end
          end  
          if !send_in && @inter_traces.key?(path)
            if @inter_traces[path][:lines].key?(line)
              send_in = true
            end
          end
          if send_in
            @inter_traces[send_position[0]][:lines][send_position[1]].each do |a|
              if a[1].kind == :Variable && a[1].label == tp.method_id
                a.typ = typ
                hack = tp.method_id == :check_body_lines
                if a[0].kind == :Nil && (@stack[-1] != '' || hack) && ![:include, :p, :String, :format, :require].include?(tp.method_id)
                  a[0] = val(:Self)
                  if tp.method_id == :check_name
                    p tp.method_id
                  end
                end
                
              end
            end
          end

          if @call_lines.length > 0
            method_line = @call_lines.pop
            if @inter_traces.key?(tp.path) && @inter_traces[tp.path][:method_lines].key?(method_line)
              kind = @inter_traces[tp.path][:method_lines][method_line].kind
              is_block = tp.event == :b_return
              if kind == :Block && is_block || kind == :NodeMethod && !is_block
                @inter_traces[tp.path][:method_lines][method_line].return_type = typ
              end
            end
          end
        end
      end

      t3 = TracePoint.new(:raise) do |tp|
        exception = tp.raised_exception
        @trace[-1][-1] = exception
      end

      $t.enable
      $t2.enable

      if load
        Kernel.load @program
      end

      at_exit do
        $t.disable
        $t2.disable

        @inter_traces = Hash[@inter_traces.reject { |path, file| path.include?('mixin') }]
        generate
      end
    end
  end
end

def deduckt
  outdir = ENV['DEDUCKT_OUTPUT_DIR']
  module_patterns = ENV['DEDUCKT_MODULE_PATTERNS'].split ' '

  Deduckt::TraceRun.new('', [], {outdir: outdir, module_patterns: module_patterns}).execute(load = false)
end
