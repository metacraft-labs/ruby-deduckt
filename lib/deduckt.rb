require "deduckt/version"
require 'json'
require 'parser/ruby25'
require 'ast'
require 'set'

module Deduckt
  TABLE_TYPE = {kind: :Generic, label: "Table", genericArgs: ["K", "V"]}
  # object types
  INTER_CLASS = 0
  INTER_METHOD = 1
  INTER_CALL = 2
  INTER_VARIABLE = 3

  PATTERN_STDLIB = {puts: -> a { {kind: INTER_CALL, children: [{kind: INTER_VARIABLE, label: "echo"}] + a , typ: {kind: :Nil} } } }

  KINDS = {lvasgn: :Assign, array: :Sequence, hash: :NimTable, begin: :Code, dstr: :Docstring, return: :Return, yield: :Yield, next: :Continue, break: :Break, False: :Bool, True: :Bool, while: :While, when: :Of, erange: :Range, zsuper: :Super, kwbegin: :Try, rescue: :Except, resbody: :Code}

  OPERATORS = Set.new [:+, :-, :*, :/, :==, :>, :<, :>=, :<=, :"!=", :"&&", :"||"]
  NORMAL = Set.new [:RubyIf, :RubyPair, :RubyCase, :RubySelf]

  class TraceRun
    def initialize(program, args, options)
      @program = program
      @args = args
      @module_patterns = options[:module_patterns]
      @outdir = options[:outdir]
      @trace = []
      @lines = []
      @inter_traces = {}
      @inter_types = {}
      @stack = []
      @call_lines = []
      @current_block = ""
      @processing = {}
    end

    def trace_calls(data)
      @call_lines.push(data.lineno)
      if @inter_traces[data.path][:method_lines].key?(data.lineno)
        if @inter_traces[data.path][:method_lines][data.lineno][:kind] == :NodeMethod && data.event != :b_call ||
           @inter_traces[data.path][:method_lines][data.lineno][:kind] == :Block && data.event == :b_call
          @stack.push('')
          @inter_traces[data.path][:method_lines][data.lineno][:args].each do |arg|
            # puts "ARG #{arg}"
            if arg[:label] == :self
              arg[:typ] = load_type(data.binding.receiver)
              @stack[-1] = arg[:typ][:label]
            else
              begin
                if arg[:typ].nil? 
                  arg[:typ] = load_type(data.binding.local_variable_get(arg[:label]))
                end
              rescue
                arg[:typ] = {kind: :Simple, label: "Void"}
              end
            end
          end
        end
      end
    end

    def load_type(arg)
      if arg.class.nil?
        return {kind: :Simple, label: "Void"}
      end

      #$t.disable
      #$t2.disable

      klass = arg.class
      # puts "TYPE #{arg} #{klass.name.to_sym}"
      if !@processing.key?(klass.name)
        @processing[klass.name] = []
        variables = arg.instance_variables.map do |a|
          load_type(arg.instance_variable_get(a)).tap { |b| b[:fieldLabel] = a.to_s[1 .. -1] }
        end
        @processing[klass.name] = variables
      else
        if klass == Integer
          return {kind: :Simple, label: "Int"}
        elsif klass == Float
          return {kind: :Simple, label: "Float"}
        elsif klass == NilClass
          return {kind: :Simple, label: "Void"}
        elsif klass == String
          return {kind: :Simple, label: "String"}
        elsif klass == TrueClass || klass == FalseClass
          return {kind: :Simple, label: "Bool"}
        elsif klass == Array
          return {kind: :Compound, original: {kind: :Simple, label: "Sequence"}, args: [load_type(arg[0])]}
        else
          return {kind: :Simple, label: klass.name.to_sym}
        end
      end
      res = if klass == Integer
        {kind: :Simple, label: "Int"}
      elsif klass == Float
        {kind: :Simple, label: "Float"}
      elsif klass == NilClass
        {kind: :Simple, label: "Void"}
      elsif klass == String
        {kind: :Simple, label: "String"}
      elsif klass == TrueClass || klass == FalseClass
        {kind: :Simple, label: "Bool"}
      elsif klass == Hash
        if arg.length == 0
          key = {kind: :Simple, label: "Void"}
          value = key
        else
          arg.each do |k, v|
            key = load_type(k)
            value = load_type(v)
            break
          end
        end
        {kind: :Compound, args: [key, value], original: TABLE_TYPE}
      elsif klass == Symbol
        {kind: :Simple, label: "Symbol"}
      else
        {kind: :Simple, label: klass.name}
      end

      # Praise the Lord!

      if klass.is_a?(Class) && variables.length > 0
        # ok for now
        if !@inter_types[res[:label].to_sym]
          label = res[:label].to_sym
          res[:kind] = :Object

          if @outdir != "test"
            res[:fields] = [] # TODO inheritance variables
          else
            res[:fields] = variables
          end
          @inter_types[label] = res
          if label.to_s.include?('::')
            # p label.to_s.split('::')[-1].to_sym
            @inter_types[label.to_s.split('::')[-1].to_sym] = res
          end
          res = {kind: :Simple, label: label} # Praise the Lord!
        end
        # far simpler to just move all the classes to %types: easier to process and search
      end

      #$t.enable
      #$t2.enable
      res
    end

    def load_method(args, return_type)
      {kind: :NodeMethod, args: args.map { |arg| load_type(arg) }, returnType: load_type(return_type)}
    end

    def union(left, right)
      case left[:kind]
      when :Method
        same = left[:args] == right[:args] and left[:returnType] == right[:returnType]
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

    def write(inputs)
      traces = {}
      paths = {}
      methods = {}
      i = 0
      method_stream = []
      inputs.each do |input|
        id = "#{input[2]}.#{input[3]}"
        if !methods.key?(id)
          methods[id] = []
        end
        if input[5] == :input
          methods[id].push([input, []])
          method_stream.push(methods[id][-1])
        else
          methods[id][-1][1] = input
          methods[id].pop
        end
      end

      method_stream.each do |input, result|
        if result.length == 0
          next
        end
        id = "#{input[2]}.#{input[3]}"
        if traces.key?(id)
          traces[id], _ = union(traces[id], load_method(input[4], result[4]), nil)
        else
          traces[id] = load_method(input[4], result[4])
        end
        if not paths.key?(input[0])
          paths[input[0]] = []
        end
        paths[input[0]].push(id)
      end

      [traces, paths]
    end

    class InterTranslator
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
            new_class = res[:classes][-1][:children].select { |it| it[:kind] != :RubyClass }
            classes = res[:classes][-1][:children].select { |it| it[:kind] == :RubyClass }
            if classes.length > 0
              res[:classes][-1][:children] = new_class
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
        i = object[:line] - 1
        while i > 2
          if @raw[i].nil? || !@raw[i].lstrip.start_with?('#')
            break
          end
          if @comments.key?(i)
            if object[:docstring].nil?
              object[:docstring] = []
            end
            object[:docstring].push(@comments[i].gsub(/\A#+/, ''))

          end
          i -= 1
        end
        object[:docstring].reverse! if !object[:docstring].nil? 

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
            value = {kind: :NodeMethod, label: {kind: :Variable, label: node.children[index]}, args: [], code: [], isIterator: false, typ: nil, returnType: nil}
            value[:args] = [{kind: :Variable, label: :self, typ: nil}] + node.children[index + 1].children.map { |it| process_node it }
            value[:code] = node.children[index + 2 .. -1].map { |it| process_node it }
            value[:line] = line
            find_docstring(value)
            value[:docstring] = value[:docstring] || []
            if node.type == :defs
              value[:isClass] = true
            end
            if @is_iterator
              value[:isIterator] = true
            end
            @inter_ast[:method_lines][line] = value
            return value.tap { |t| t[:line] = line; t[:column] = column }
          elsif node.type == :block
            value = {kind: :Block, label: {kind: :Variable, label: ""}, args: [], code: [], typ: nil, returnType: nil}
            value[:args] = node.children[1].children.map { |it| process_node it }
            value[:code] = node.children[2 .. -1].map { |it| process_node it }
            @inter_ast[:method_lines][line] = value
            child = process_node node.children[0]
            child[:children].push(value)
            return child.tap { |t| t[:line] = line; t[:column] = column }
          end
          value = {kind: get_kind(node.type), children: node.children.map { |it| process_node it }, typ: nil}
          if node.type == :send
            if value[:children][0][:kind] == :RubyConst && (value[:children][1][:kind] == :Variable && value[:children][1][:label] == :new || value[:children][1][:kind] == :Symbol && value[:children][1][:text] == :new)
              value = {kind: :New, children: [{kind: :Variable, label: value[:children][0][:label]}] + value[:children][2 .. -1]}
            end
            if value[:children].length > 1
              value[:children][1][:kind] = :Variable
              value[:children][1][:label] = value[:children][1][:text]
              value[:children][1].delete :text
            end
            if !@inter_ast[:lines].key?(line)
              @inter_ast[:lines][line] = []
            end
            @inter_ast[:lines][line].push(value)
            value
          else
            if node.type == :if && node.children[1].nil?
              value[:children][0] = {kind: :UnaryOp, children: [{kind: :Operator, label: "not"}, value[:children][0]], typ: {kind: :Simple, label: "Bool"}}
              value[:children][1] = value[:children][2]
              value[:children].pop
            end
            value
          end.tap { |t| if !node.nil?; t[:line] = line; t[:column] = column; end }
        elsif node.class == Integer
          {kind: :Int, i: node, typ: nil}
        elsif node.class == Float
          {kind: :Float, f: node, typ: nil}
        elsif node.class == String
          {kind: :String, text: node, typ: nil}
        elsif node.class == Symbol
          {kind: :Symbol, text: node, typ: nil}
        elsif node.nil?
          {kind: :Nil, typ: nil}
        end
      end

      def process_const(node)
        {kind: :RubyConst, label: node.children[1]}
      end

      def process_op_asgn(node)
        left = process_node(node.children[0].children[0])
        op = {kind: :Operator, label: node.children[1]}
        right = process_node(node.children[2])
        if left[:kind] == :Symbol
          left[:kind] = :Variable
          left[:label] = left[:text]
        end
        {kind: :AugOp, children: [left, op, right]}
      end

      def process_or_asgn(node)
        left = process_node(node.children[0].children[0])
        op = {kind: :Operator, label: "or"}
        right = process_node(node.children[1])
        if left[:kind] == :Symbol
          left[:kind] = :Variable
          left[:label] = left[:text]
        end
        {kind: :Assign, children: [left, {kind: :BinOp, children: [left, op, right]}]}
      end

      def process_int(node)
        {kind: :Int, i: node.children[0]}
      end

      def process_str(node)
        {kind: :String, text: node.children[0]}
      end

      def process_dstr(node)
        {kind: :Docstring, text: node.children.map { |it| it.children[0] }.join }
      end

      def process_float(node)
        {kind: :Float, f: node.children[0]}
      end

      def process_sym(node)
        {kind: :Symbol, text: node.children[0]}
      end

      def process_true(node)
        {kind: :Bool, val: true}
      end

      def process_false(node)
        {kind: :Bool, val: false}
      end

      def process_and(node)
        {kind: :BinOp, children: [{kind: :Operator, label: "and"}, process_node(node.children[0]), process_node(node.children[1])]}
      end

      def process_or(node)
        {kind: :BinOp, children: [{kind: :Operator, label: "or"}, process_node(node.children[0]), process_node(node.children[1])]}
      end

      def process_ivar(node)
        {kind: :Attribute, children: [{kind: :Self}, {kind: :String, text: node.children[0][1 .. -1]}]}
      end

      def process_lvar(node)
        {kind: :Variable, label: node.children[0]}
      end

      def process_gvar(node)
        {kind: :Variable, label: node.children[0]}
      end

      def process_ivasgn(node)
        {kind: :Assign, children: [{kind: :Attribute, children: [{kind: :Self}, {kind: :String, text: node.children[0][1 .. -1]}]}, process_node(node.children[1])]}
      end

      def process_lvasgn(node)
        {kind: :Assign, children: [{kind: :Variable, label: node.children[0]}, process_node(node.children[1])]}
      end

      def process_block_pass(node)
        arg = node.children[0].children[0].nil? ? "" : node.children[0].children[0][0 .. -1]
        {kind: :Block, args: [{kind: :Variable, label: :it}], code: [{kind: :Attribute, children: [{kind: :Variable, label: :it}, {kind: :String, text: arg}]}]}
      end

      def process_casgn(node)
        value = node.children[-1]
        if value.type == :send && value.children[-1] == :freeze
          value = value.children[0]
        end
        # how to do it when global and several have the same name? label = "#{@current_class}#{node.children[1]}"
        label = node.children[1]
        {kind: :Assign, children: [{kind: :Variable, label: label}, process_node(value)], declaration: :Const}
      end

      def process_nil(node)
        {kind: :Nil}
      end

      def process_arg(node)
        {kind: :Variable, label: node.children[0]}
      end

      def process_module(node)
        process_node(node.children[1])
      end

      def process_masgn(node)
        if node.children[0].children.length == 1
          right = {kind: :Index, children: [node.children[1].type == :send ? process_node(node.children[1]) : process_node(node.children[1].children[0].children[0]), {kind: :Int, i: 0}]}
          {kind: :Assign, children: [{kind: :Variable, label: node.children[0].children[0].children[-1]}, right]}
        else
          {kind: :Nil}
        end
      end
    end


    def generate_ast(path)
      input = File.read(path)
      ast, comments = Parser::Ruby25.parse_with_comments(input)
      InterTranslator.new(ast, comments, input).process
    end


    def compile_child child
      if child.nil?
        return {kind: :Nil}
      end
      if child[:kind] == :RubySend
        m = if child[:children][0][:kind] == :Nil
          arg_index = 1
          if !child[:typ].nil? || !child[:children][2].nil?
            {kind: :Call, children: child[:children][1 .. -1].map { |l| compile_child l }, typ: child[:typ]}
          else
            arg_index = -1
            child[:children][1]
          end
        else
          m = if !child[:typ].nil? || !child[:children][2].nil?
            arg_index = 2
            {kind: :Send, children: child[:children].map { |l| compile_child l }, typ: child[:typ]}
          else
            arg_index = -1
            {kind: :Attribute, children: child[:children].map { |l| compile_child l }, typ: child[:typ]}
          end
          if m[:children][1][:kind] == :Variable
            m[:children][1] = {kind: :String, text: m[:children][1][:label]}
          end
          if m[:kind] == :Send && OPERATORS.include?(m[:children][1][:text].to_sym)
            arg_index = -1
            op = m[:children][1]
            op[:kind] = :Operator
            op[:label] = op[:text]
            m = {kind: :BinOp, children: [op, m[:children][0], m[:children][2]], typ: m[:typ]}
          elsif m[:kind] == :Send && m[:children][1][:text].to_sym == :[]
            m = {kind: :Index, children: [m[:children][0], m[:children][2]], typ: m[:typ]}
          end
          m
        end
        if arg_index != -1
          new_args = []

          m[:children][arg_index .. -1].each do |arg|
            if arg[:kind] == :NimTable
              new_args += arg[:children]
            else
              new_args.push(arg)
            end
          end

          m[:children] = m[:children][0 .. arg_index - 1] + new_args
        end
        m.tap { |t| t[:line] = child[:line]; t[:column] = child[:column] }
      elsif child.key?(:children)
        res = child
        res[:children] = child[:children].map { |it| compile_child it }
        if NORMAL.include?(res[:kind])
          res[:kind] = res[:kind].to_s[4 .. -1].to_sym
        end
        res
      elsif child.key?(:code)
        res = child
        res[:code] = child[:code].map { |it| compile_child it }
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
          compile_child(child)
        end

        file[:classes] = file[:classes].map do |klass|
          parent = klass[:children][1]
          if parent.nil? || parent[:kind] == :Nil
            parent = {kind: :Simple, label: "Void"}
          elsif parent[:kind] == :RubyConst
            parent = {kind: :Simple, label: parent[:label]}
          end
          mercy = klass[:children][2]
          if mercy[:kind] == :RubyBegin
            mercy = mercy[:children]
          else
            mercy = klass[:children][2 .. -1]
          end
          if mercy.length == 1 && mercy[0][:kind] == :Code
            children = mercy[0][:children]
            mercy = mercy[0][:children].select { |n| n[:kind] == :NodeMethod }
            b = children.select { |n| n[:kind] != :NodeMethod }
            file[:main] += b.map { |it| compile_child(it) }
          end
          if @inter_types.key?(klass[:children][0][:label])
            @inter_types[klass[:children][0][:label]][:base] = parent
          end
          $types_no_definition.delete klass[:children][0][:label]

          {kind: :Class,
           label: klass[:children][0][:label],
           methods: mercy.map { |met| {label: met[:label][:label], node: compile_child(met)} },
           fields: [],
           docstring: klass[:docstring] || [],
           typ: @inter_types[klass[:children][0][:label]]}
        end
        
      end
      @inter_traces['%types'] = $types_no_definition
    end

    def generate
      compile @inter_traces
      #p @outdir
      #p @inter_traces.length
      File.write(File.join(@outdir, "lang_traces.json"), JSON.pretty_generate(@inter_traces))
    end

    def module_of_interest?(path)
      if path.include?('mixin')
        return true
      end
      for pattern in @module_patterns
        if pattern == 'rubocop'
          return false if not path.include?(pattern)
        else
          return false if not path.include?('/' + pattern)
        end
      end
      true
    end

    def execute(load=true)
      trace_run = self

      $t = TracePoint.new(:call, :c_call, :b_call) do |tp|
        next if tp.method_id == :method_added or not trace_run.module_of_interest?(tp.path)

        # TODO require require_relative

        if ![:new, :initialize, :enable, :disable, :require_relative, :require].include?(tp.method_id) #tp.path != "deduckt.rb" &&
          if !@inter_traces.key?(tp.path)
            @inter_traces[tp.path] = generate_ast(tp.path)
          end
          trace_calls(tp)
        end
      end

      $t2 = TracePoint.new(:return, :c_return, :b_return) do |tp|
        next if tp.method_id == :method_added or not trace_run.module_of_interest?(tp.path)

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
              if a[:children][1][:kind] == :Variable && a[:children][1][:label] == tp.method_id
                a[:typ] = typ
                hack = tp.method_id == :check_body_lines
                if a[:children][0][:kind] == :Nil && (@stack[-1] != '' || hack) && ![:include, :p, :String, :format, :require].include?(tp.method_id)
                  a[:children][0] = {kind: :Self}
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
              kind = @inter_traces[tp.path][:method_lines][method_line][:kind]
              is_block = tp.event == :b_return
              if kind == :Block && is_block || kind == :NodeMethod && !is_block
                @inter_traces[tp.path][:method_lines][method_line][:returnType] = typ
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

        @inter_traces = Hash[@inter_traces.select { |path, file| !path.include?('mixin') }]
        generate
      end
    end
  end
end

def deduckt
  outdir = ENV["DEDUCKT_OUTPUT_DIR"]
  module_patterns = ENV["DEDUCKT_MODULE_PATTERNS"].split ' '

  Deduckt::TraceRun.new("", [], {outdir: outdir, module_patterns: module_patterns}).execute(load=false)
end
