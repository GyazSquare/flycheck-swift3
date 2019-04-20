protocol P0 {
  associatedtype A

  func foo()

  var prop: A { get }
}

protocol P1: P0 {
  associatedtype A

  func foo()

  var prop: A { get }
}

// Silence warnings with @_nonoveride.
protocol P2: P0 {
  @_nonoverride
  associatedtype A

  @_nonoverride
  func foo()

  @_nonoverride
  var prop: A { get }
}
