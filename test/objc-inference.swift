import Foundation

class MySuperclass: NSObject {}

extension MySuperclass {
    func extMethod() {}
}

class MySubclass: MySuperclass {
    override func extMethod() {}
}
