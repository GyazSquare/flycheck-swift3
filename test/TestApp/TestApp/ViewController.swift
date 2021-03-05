//
//  ViewController.swift
//  TestApp
//
//  Created by John on 25/10/2018.
//  Copyright © 2018 Test Corporation Ltd. All rights reserved.
//

import Cocoa

class ViewController: NSViewController {
    
    let foo = bar // error

    override func viewDidLoad() {
        super.viewDidLoad()

        // Do any additional setup after loading the view.
    }

    override var representedObject: Any? {
        didSet {
        // Update the view, if already loaded.
        }
    }


}

