type IA =
    abstract member Go: unit -> int

type IB =
    abstract member Go: unit -> string


type TypeList =
    abstract member Get: unit -> 'f

and Nil =
    | Nil

    interface TypeList

and Cons<'a, 'b when 'b :> TypeList> =
    | Cons of 'a * 'b

    interface TypeList

and Hole<'i, 'r> =
    | Hole of ('i -> 'r)

    interface TypeList

and Fill<'i, 'r> =
    | Fill of 'i * ('i -> 'r)

    interface TypeList

and Provider = | Provide

and Resolve =
    private
    | Resolve

    static member inline ($)(Resolve, (Hole(fn): Hole<'i, 'r>, i: 'i)) : Fill<'i, 'r> = Fill(i, fn)

    //static member inline ($)(Resolve, (Cons(Hole(fn), tail): Cons<Hole<'i, 'r>, _>, i: 'i)) : Cons<Fill<'i, 'r>, _> =
    //    Cons(Fill(i, fn), tail)

    //static member inline (|*)(Resolve, (Hole(fn): Hole<'iOther, 'r>, _: 'i)) : Hole<'iOther, 'r> = Hole(fn)

    static member inline ($)(Resolve, (Fill(i, fn): Fill<'i, 'r>, _: 'i)) : Fill<'i, 'r> = Fill(i, fn)
    //static member inline (|*)(Resolve, (Fill(i, fn): Fill<'iOther, 'r>, _: 'i)) : Fill<'iOther, 'r> = Fill(i, fn)

    static member inline ($)(Resolve, (Nil: Nil, _: 'i)) : Nil = Nil

[<AutoOpen>]
module LowPriority =
    type Provider with

        static member inline op_Explicit
            (
                Provide,
                f,
                recurse: ('a * 'i) -> #TypeList,
                (Cons(hole, tail): Cons<Hole<'iOther, 'r>, 'a>, i: 'i)
            ) : Cons<Hole<'iOther, 'r>, 'a> =
            let newHead = hole
            let newTail = f $ (recurse (tail, i))
            Cons(newHead, newTail)

        static member inline op_Explicit
            (
                Provide,
                f,
                recurse: ('a * 'i) -> #TypeList,
                ((Cons(fill, tail)): Cons<Fill<'iOther, 'r>, 'a>, i: 'i)
            ) : Cons<Fill<'iOther, 'r>, 'a> =
            let newHead = fill
            let newTail = f $ (recurse (tail, i))
            Cons(newHead, newTail)

//static member inline op_Explicit
//    (
//        Provide,
//        f,
//        recurse: ('a * 'i) -> #TypeList,
//        (Cons(head, tail): Cons<'c, 'a>, i: 'i)
//    ) : Cons<#TypeList, #TypeList> =
//    let newHead = f $ (head, i)
//    let newTail = f $ (recurse (tail, i))
//    Cons(newHead, newTail)


//static member inline op_Explicit
//    (
//        Provide,
//        f,
//        (Cons(head, _): Cons<_, Nil>, _: 'i)
//    ) : Cons<_, Nil> =
//    Cons(head, Nil)

[<AutoOpen>]
module HighPriority =
    type Provider with

        static member inline op_Explicit
            (
                Provide,
                f,
                recurse: ('a * 'i) -> #TypeList,
                (Cons(hole, tail): Cons<Hole<'i, 'r>, 'a>, i: 'i)
            ) : Cons<Fill<'i, 'r>, 'a> =
            let newHead = f $ (hole, i)
            let newTail = f $ (recurse (tail, i))
            Cons(newHead, newTail)

        static member inline op_Explicit
            (
                Provide,
                f,
                recurse: ('a * 'i) -> #TypeList,
                ((Cons(fill, tail)): Cons<Fill<'i, 'r>, 'a>, i: 'i)
            ) : Cons<Fill<'i, 'r>, 'a> =
            let newHead = f $ (fill, i)
            let newTail = f $ (recurse (tail, i))
            Cons(newHead, newTail)



//static member inline op_Explicit
//    (
//        Provide,
//        f,
//        (Cons(head, _): Cons<Hole<'iOther, 'r>, Nil>, _: 'i)
//    ) : Cons<Hole<'iOther, 'r>, Nil> =
//    Cons(head, Nil)

//static member inline op_Explicit
//    (
//        Provide,
//        f,
//        (Cons(head, _): Cons<Fill<IA, int>, Nil>, _: IA)
//    ) : Cons<Fill<IA, int>, Nil> =
//    Cons(head, Nil)

//static member inline op_Explicit
//    (
//        Provide,
//        f,
//        (Cons(head, _): Cons<Fill<IA, int>, Nil>, _: IA)
//    ) : Cons<Fill<IA, int>, Nil> =
//    Cons(head, Nil)



let inline (^+^) head tail = Cons(head, tail)

//// Examples

let A =
    { new IA with
        member _.Go() = 42 }

let B =
    { new IB with
        member _.Go() = "Hello World!" }

type IBOTH =
    inherit IA
    inherit IB

let BOTH =
    { new IBOTH with
        member _.Go() = 42
        member _.Go() = "Hello World!" }

let ahole = Hole(fun (i: #IA) -> i.Go())
let bhole = Hole(fun (i: #IB) -> i.Go())

let double () = ahole ^+^ ahole ^+^ Nil

let asList () = ahole ^+^ Nil
let bsList () = bhole ^+^ Nil
let absList () = ahole ^+^ bhole ^+^ Nil


let rec inline recurse (list: 'a, i) =
    Provider.op_Explicit (Provide, Resolve, recurse, (list, i))

//let t1 =
//    Provider.op_Explicit (Provide, Resolve, recurse, (Cons(Hole((fun _ -> ())), Nil), 2))

//let t1 =
//    Provider.op_Explicit (Provide, Resolve, recurse, (Cons(Fill(2, (fun _ -> ())), Nil), 2))

let asa () =
    Provider.op_Explicit (Provide, Resolve, recurse, (asList (), A))

let asb () =
    Provider.op_Explicit (Provide, Resolve, recurse, (asList (), B))

let bsa () =
    Provider.op_Explicit (Provide, Resolve, recurse, (bsList (), A))

let bsb () =
    Provider.op_Explicit (Provide, Resolve, recurse, (bsList (), B))

let absa () =
    Provider.op_Explicit (Provide, Resolve, recurse, (absList (), A))

let absb () =
    Provider.op_Explicit (Provide, Resolve, recurse, (absList (), B))

//let next () = Provider.op_Explicit (Provide, Resolve, recurse, (absb (), A))
