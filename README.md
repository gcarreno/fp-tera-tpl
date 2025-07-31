# Tera Templates
[![Supports Windows](https://img.shields.io/badge/support-Windows-blue?logo=Windows)](https://github.com/gcarreno/fp-tera-tpl/releases/latest)
[![Supports Linux](https://img.shields.io/badge/support-Linux-yellow?logo=Linux)](https://github.com/gcarreno/fp-tera-tpl/releases/latest)
[![Supports macOS](https://img.shields.io/badge/support-macOS-black?logo=macOS)](https://github.com/gcarreno/fp-tera-tpl/releases/latest)
[![License](https://img.shields.io/github/license/gcarreno/fp-tera-tpl?logo=github)](https://github.com/gcarreno/fp-tera-tpl/blob/main/LICENSE)
[![Latest Release](https://img.shields.io/github/v/release/gcarreno/fp-tera-tpl?label=latest%20release&logo=github)](https://github.com/gcarreno/fp-tera-tpl/releases/latest)
[![Downloads](https://img.shields.io/github/downloads/gcarreno/fp-tera-tpl/total?logo=github)](https://github.com/gcarreno/fp-tera-tpl/releases)

This package is an attempt to port the famous Tera template engine into Free Pascal.

## Objectives

- [x] Implement full `AST` on the engine.
- [x] Implement `{{ <variable> }}`.
- [x] Implement filters: `{{ <variable> | <filter> | <filter> }}`.
- [x] Implement `{% include  "<filename>" %}`.
- [x] Implement `{% if <expression> %}`, `{% else %}` and `{% endif %}`.
- [x] Implement decent expression parser for `{% if <expression> %}` with `AST`.
- [x] Implement `{% for <variable> in <variable_array> %}` and `{% enfor %}`.
- [ ] Implement full battery of tests.
- [ ] Implement `{%-` and `-%}` for `TrimLeft` and `TrimRight`.

## Examples

There are examples: [Examples Folder](examples)

## Package

You can find the package here: [Packages Folder](packages)
