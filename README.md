<p align="center">
  <a href="" rel="noopener">
 <img height=64px src="assets/love.png" alt="Project logo"></a>
</p>

<h1 align="center">Love Programming Language</h1>
<p align="center">
A simple imperative programming language which will bring love to your life!
</p> 
<br />

```go
func main() {
  print("Hello, world!")
}
```
<br />

<div align="center">

[![Status](https://img.shields.io/badge/status-active-success.svg)]()
[![GitHub Issues](https://img.shields.io/github/issues/thomasvergne/love.svg)](https://github.com/quark-lang/quark/issues)
[![GitHub Pull Requests](https://img.shields.io/github/issues-pr/thomasvergne/love.svg)](https://github.com/quark-lang/quark/pulls)
[![License](https://img.shields.io/badge/license-Creative%20commons-blue.svg)](/LICENSE)
</div>

## Modern features
- **Dynamically typed** for easy prototyping
- **Functional features** for efficient programming
- **Object implementation** for basic and easy object-oriented use
- **Easy syntax** for easy learning
- And much more...

## Some examples of Love

### Constants
Constants are a compile-time feature. They must be pure values and can only be assigned once.

```go
const x := 5
const y := 6
print(x + y)
```

### Variables
```go
x := 6
print(x)
x = 2
print(x)
```

### Functions
Functions extend from functional programming as there are lambdas (AKA anonymous functions) and closures. 
```go
func add(x, y) x + y
func sub(x, y) {
  return x - y
}

main := func() {
  print(add(5, sub(2, 3)))
}
```

### Structures
Structures are the way to do some object-oriented programming in the language. They are similar to javascript objects.
```go
person := struct {
  name: "John Doe",
  age: 37
}
print(person.name)
```