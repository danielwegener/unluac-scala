
This is a scala port of http://sourceforge.net/projects/unluac/ enabling you to decompile lua bytecode in the javascript land via scalajs.

It is not meant to be seriously used but just as part of a bigger demonstration what kind of stuff could run in a browser. It may be interesting to be used in combination with lua.vm.js or moonshine.

Unfortunately the port started with an old version of unluac, so it is pretty much behind the upstream project.

Disclaimer: The code has been ported by IntelliJ's java to scala code converter plus a lot of handcrafting, so please bear with me if I introduced any bugs. I have absolutely no idea how the underlying decompiler actually works and just tried to keep the tests green while porting the codebase to scala using equivalent transformations.

This port is MIT licensed (like the original unluac project).
