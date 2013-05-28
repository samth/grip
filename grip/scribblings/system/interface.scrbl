#lang scribble/manual

@begin[(require (for-label grip/system/interface)
		(for-label (only-meta-in 0 typed/racket)))]

@title{Network}

@section{Interface}

@defmodule[grip/system/interface]{

  @defproc[(interface-mac-address [name String]) String]
   
   The MAC address for a named interface, e.g. eth0, wlan0, ...
   
   @defproc[(named-interfaces) (Listof String)]
   
   The interfaces defined on the system.  A listing of directory /sys/class/net.
}
