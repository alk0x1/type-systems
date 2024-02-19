module Context = Map.Make(String)

type expr =
  | Variable of string
  | Abstraction of { param: string; body: expr }
  | Application of { funct: expr; argument: expr }

type value =
  | Closure of { context: value Context.t; param: string; body: expr }
  | Native of (value -> value)

let rec interpreter context expr =
  match expr with
  | Variable name -> 
      Context.find name context
  | Abstraction { param; body } -> 
      Closure { context; param; body }
  | Application { funct; argument } ->
      let argument_val = interpreter context argument in
      match interpreter context funct with
      | Closure { context = fn_context; param; body } -> 
          interpreter (Context.add param argument_val fn_context) body
      | Native f -> 
          f argument_val

let f =
  Abstraction {
    param = "x";
    body = Application {
      funct = Variable "x";
      argument = Application {
        funct = Variable "print_hello_world";
        argument = Variable "x";
      }
    }
  }

let code = Application { funct = f; argument = f }

let initial_context =
  Context.empty
  |> Context.add "print_hello_world" (Native (fun v ->
       print_endline "hello world";
       v))

let _ = interpreter initial_context code
