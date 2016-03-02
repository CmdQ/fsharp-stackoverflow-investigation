module GeneticLbp.SafeNativeMethods

open System.Runtime.InteropServices

type ConsoleCloseHandler = delegate of int -> bool

[<DllImport("Kernel32")>]
extern bool SetConsoleCtrlHandler(ConsoleCloseHandler handler, bool add)
