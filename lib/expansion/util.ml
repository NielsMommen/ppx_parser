let peek ~loc = [%expr Stream.peek ppx____parser____stream____]
let junk cont ~loc = [%expr let () = Stream.junk ppx____parser____stream____ in [%e cont]]

let some_pat pat ~loc = [%pat? Some ([%p pat])]

let raise_fail_exn ~loc = [%expr raise Stream.Failure]

let raise_err_exn ~loc = [%expr raise (Stream.Error "Parse error.")]