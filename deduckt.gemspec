
lib = File.expand_path("../lib", __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require "deduckt/version"

Gem::Specification.new do |spec|
  spec.name          = "deduckt"
  spec.version       = Deduckt::VERSION
  spec.authors       = ["Alexander Ivanov", "Zahary Karadjov"]
  spec.email         = ["alexander@metacraft-labs.com", "zahary@metacraft-labs.com"]

  spec.summary       = %q{Extract type information from Ruby programs through run-time analysis.}
  spec.description   = %q{Through the powers of deductive interpretation, Deduckt will obtain the precise call graph of your Ruby programs and the type signatures of all executed functions. Created for the goals of ruby2nim.}

  spec.homepage      = "https://github.com/metacraft-labs/ruby-deduckt"
  spec.license       = "MIT"

  spec.files         = `git ls-files -z`.split("\x0").reject do |f|
    f.match(%r{^(test|spec|features)/})
  end
  spec.bindir        = "exe"
  spec.executables   = spec.files.grep(%r{^exe/}) { |f| File.basename(f) }
  spec.require_paths = ["lib"]

  spec.add_development_dependency "bundler", "~> 1.16"
  spec.add_development_dependency "rake", "~> 10.0"
  spec.add_development_dependency "rspec", "~> 3.0"

  spec.add_runtime_dependency "parser"
end
