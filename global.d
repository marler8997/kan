module global;

import std.array : Appender;

import mod : Module;

ubyte verbose;
bool willOptimize;
Appender!(string[]) importPaths;
Appender!(Module[]) modules;
