# houseman[![Circle CI](https://circleci.com/gh/fujimura/houseman.svg?style=shield)](https://circleci.com/gh/fujimura/houseman)

[![Join the chat at https://gitter.im/fujimura/houseman](https://badges.gitter.im/fujimura/houseman.svg)](https://gitter.im/fujimura/houseman?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

A Haskell implementation of [Foreman](https://github.com/ddollar/foreman)

## Usage

### Start with Procfile
```
$ houseman start
```

### Run one-off command

```
$ houseman run --command your_command
```

See other options by

```
$ houseman start --help
$ houseman run --help
```

## Installation

Get [Stack](http://haskellstack.org) to run Haskell compilier, and

```
$ git clone https://github.com/fujimura/houseman.git
$ cd houseman
$ stack install
```

## Contributing

1. Fork it ( http://github.com/fujimura/houseman/fork )
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
