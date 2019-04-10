# ruby-deduckt

Through the powers of deductive interpretation, Deduckt will obtain the precise
call graph of your Ruby programs and the type signatures of all executed functions.
Created for the goals of [ruby2nim](https://github.com/metacraft-labs/ruby2nim).

## Installation

    $ gem install deduckt

## How to Use

`ruby-deduckt` works as a drop-in replacement for the Ruby interpreter. To obtain the full type information about a particular program, execute it like this:

    $ ruby-deduckt program.rb [args]

## Output

TBD

## Contributing

```bash
  bundle config local.deduckt .
  bundle exec deduckt/ruby-deduckt
```

bnd
## FAQ

* Why does it analyze all the events?

In the future, we might add an option to trace only a statistically significant portion of the events. We don't consider this a priority for our use case ([ruby2nim](https://github.com/metacraft-labs/py2nim)) because a porting task is not something that would be ran often and better type information is more important than speed.

* How to pronounce deduckt?

Like the first part of "deductive", or like getting rid of the duck types in your backyard. 

## Contributing

After checking out the repo, run `bin/setup` to install dependencies. Then, run `rake spec` to run the tests. You can also run `bin/console` for an interactive prompt that will allow you to experiment.

## License

The gem is available as open source under the terms of the [MIT License](https://opensource.org/licenses/MIT).
