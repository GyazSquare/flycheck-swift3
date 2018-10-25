//
//  AppDelegate.swift
//  TestApp
//
//  Created by John on 25/10/2018.
//  Copyright © 2018 Test Corporation Ltd. All rights reserved.
//

import Cocoa

@NSApplicationMain
class AppDelegate: NSObject, NSApplicationDelegate {

    var viewController: ViewController?

    func applicationDidFinishLaunching(_ aNotification: Notification) {
        // Insert code here to initialize your application
        print("applicationDidFinishLaunching")
        print("contentViewController:", NSApp.mainWindow?.contentViewController ?? "nil")
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        // Insert code here to tear down your application
    }

}

