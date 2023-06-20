type IA =
    abstract member Go: unit -> int

type IB =
    abstract member Go: unit -> string


type TypeList =
    interface
    end

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
and Recurser = | Recurser

and Resolve =
    private
    | Resolve

    static member inline (|*)(Resolve, (Hole(fn): Hole<'i, 'r>, i: 'i)) : Fill<'i, 'r> = Fill(i, fn)
    static member inline (|**)(Resolve, (Hole(fn): Hole<'i, 'r>, i: 'i)) : Fill<'i, 'r> = Fill(i, fn)
    static member inline (|***<)(Resolve, (Hole(fn): Hole<'i, 'r>, i: 'i)) : Fill<'i, 'r> = Fill(i, fn)
    static member inline (|***>)(Resolve, (Hole(fn): Hole<'i, 'r>, i: 'i)) : Fill<'i, 'r> = Fill(i, fn)
    static member inline (|****<)(Resolve, (Hole(fn): Hole<'i, 'r>, i: 'i)) : Fill<'i, 'r> = Fill(i, fn)
    static member inline (|****>)(Resolve, (Hole(fn): Hole<'i, 'r>, i: 'i)) : Fill<'i, 'r> = Fill(i, fn)

    //static member inline ($)(Resolve, (Cons(Hole(fn), tail): Cons<Hole<'i, 'r>, _>, i: 'i)) : Cons<Fill<'i, 'r>, _> =
    //    Cons(Fill(i, fn), tail)

    //static member inline (|*)(Resolve, (Hole(fn): Hole<'iOther, 'r>, _: 'i)) : Hole<'iOther, 'r> = Hole(fn)

    static member inline (|*)(Resolve, (Fill(i, fn): Fill<'i, 'r>, _: 'i)) : Fill<'i, 'r> = Fill(i, fn)
    static member inline (|**)(Resolve, (Fill(i, fn): Fill<'i, 'r>, _: 'i)) : Fill<'i, 'r> = Fill(i, fn)
    static member inline (|***<)(Resolve, (Fill(i, fn): Fill<'i, 'r>, _: 'i)) : Fill<'i, 'r> = Fill(i, fn)
    static member inline (|***>)(Resolve, (Fill(i, fn): Fill<'i, 'r>, _: 'i)) : Fill<'i, 'r> = Fill(i, fn)
    static member inline (|****<)(Resolve, (Fill(i, fn): Fill<'i, 'r>, _: 'i)) : Fill<'i, 'r> = Fill(i, fn)
    static member inline (|****>)(Resolve, (Fill(i, fn): Fill<'i, 'r>, _: 'i)) : Fill<'i, 'r> = Fill(i, fn)
    //static member inline (|*)(Resolve, (Fill(i, fn): Fill<'iOther, 'r>, _: 'i)) : Fill<'iOther, 'r> = Fill(i, fn)

    static member inline (|*)(Resolve, (Nil: Nil, _: 'i)) : Nil = Nil
    static member inline (|**)(Resolve, (Nil: Nil, _: 'i)) : Nil = Nil
    static member inline (|***<)(Resolve, (Nil: Nil, _: 'i)) : Nil = Nil
    static member inline (|***>)(Resolve, (Nil: Nil, _: 'i)) : Nil = Nil
    static member inline (|****<)(Resolve, (Nil: Nil, _: 'i)) : Nil = Nil
    static member inline (|****>)(Resolve, (Nil: Nil, _: 'i)) : Nil = Nil


[<AutoOpen>]
module NilLowPriority =
    type Provider with

        //static member inline op_Explicit
        //    (
        //        Provide,
        //        f,
        //        _,
        //        (Cons(head, _): Cons<Hole<'iOther, 'r>, Nil>, i: 'i)
        //    ) : Cons<Hole<'iOther, 'r>, Nil> =
        //    Cons(head, Nil)
        static member inline op_Explicit(Provide, f, _, (Nil: Nil, i: 'i)) : Nil = Nil

//[<AutoOpen>]
//module NilHighPriority =
//    type Provider with

//        static member inline op_Explicit
//            (
//                Provide,
//                f,
//                _,
//                (Cons(head, _): Cons<Hole<'i, 'r>, Nil>, i: 'i)
//            ) : Cons<Hole<'i, 'r>, Nil> =
//            let newHead = f |***< (head, i)
//            Cons(newHead, Nil)

[<AutoOpen>]
module LowPriority =
    type Provider with

        static member inline op_Explicit
            (
                Provide,
                f,
                recurse: ('a -> 'i -> 'b),
                (Cons(hole, tail): Cons<Hole<'iOther, 'r>, 'a>, i: 'i)
            ) : Cons<Hole<'iOther, 'r>, 'b> =
            let newHead = hole
            let newTail: 'b = recurse tail i
            Cons(newHead, newTail)

        static member inline op_Explicit
            (
                Provide,
                f,
                recurse: ('a -> 'i -> 'b),
                ((Cons(fill, tail)): Cons<Fill<'i, 'r>, 'a>, i: 'i)
            ) : Cons<Fill<'i, 'r>, 'b> =
            let newHead = fill
            let newTail: 'b = recurse tail i
            Cons(newHead, newTail)

        static member inline op_Explicit
            (
                Provide,
                f,
                recurse: ('a -> 'i -> 'b),
                ((Cons(fill, tail)): Cons<Fill<'iOther, 'r>, 'a>, i: 'i)
            ) : Cons<Fill<'iOther, 'r>, 'b> =
            let newHead = fill
            let newTail: 'b = recurse tail i
            Cons(newHead, newTail)

[<AutoOpen>]
module HigherPriority =
    type Provider with

        static member inline op_Explicit
            (
                Provide,
                f,
                recurse: ('a -> 'i -> 'b),
                (Cons(hole, tail): Cons<Hole<'i, 'r>, 'a>, i: 'i)
            ) : Cons<Fill<'i, 'r>, 'b> =
            let newHead = f |***< (hole, i)
            let newTail: 'b = recurse tail i
            Cons(newHead, newTail)



//static member inline op_Explicit
//    (
//        Provide,
//        f,
//        recurse,
//        (Cons(head, tail): Cons<#NotNil, #NotNil>, i: 'i)
//    ) : Cons<#TypeList, #TypeList> =
//    let newHead = f |***< (head, i)
//    let newTail = f |***> (recurse tail i, i)
//    Cons(newHead, newTail)





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




//[<AutoOpen>]
//module LowPriority2 =
//    type Recurser with

//        static member inline op_Explicit
//            (
//                Recurser,
//                f,
//                (Cons(hole, tail): Cons<Hole<'iOther, 'r>, 'a>, i: 'i)
//            ) : Cons<Hole<'iOther, 'r>, 'b> =
//            Provider.op_Explicit (Provide, f, Recurser.op_Explicit, (tail, i))

//        static member inline op_Explicit
//            (
//                Provide,
//                f,
//                recurse: ('a * 'i) -> 'b,
//                ((Cons(fill, tail)): Cons<Fill<'iOther, 'r>, 'a>, i: 'i)
//            ) : Cons<Fill<'iOther, 'r>, 'b> =
//            let newHead = fill
//            let newTail = f |** (recurse (tail, i))
//            Cons(newHead, newTail)

////static member inline op_Explicit
////    (
////        Provide,
////        f,
////        recurse: ('a * 'i) -> #TypeList,
////        (Cons(head, tail): Cons<'c, 'a>, i: 'i)
////    ) : Cons<#TypeList, #TypeList> =
////    let newHead = f $ (head, i)
////    let newTail = f $ (recurse (tail, i))
////    Cons(newHead, newTail)


////static member inline op_Explicit
////    (
////        Provide,
////        f,
////        (Cons(head, _): Cons<_, Nil>, _: 'i)
////    ) : Cons<_, Nil> =
////    Cons(head, Nil)

//[<AutoOpen>]
//module HighPriority2 =
//    type Provider with

//        static member inline op_Explicit
//            (
//                Provide,
//                f,
//                recurse: ('a * 'i) -> 'b,
//                (Cons(hole, tail): Cons<Hole<'i, 'r>, 'a>, i: 'i)
//            ) : Cons<Fill<'i, 'r>, 'b> =
//            let newHead = f |***< (hole, i)
//            let newTail = f |***> (recurse (tail, i))
//            Cons(newHead, newTail)

//        static member inline op_Explicit
//            (
//                Provide,
//                f,
//                recurse: ('a * 'i) -> 'b,
//                ((Cons(fill, tail)): Cons<Fill<'i, 'r>, 'a>, i: 'i)
//            ) : Cons<Fill<'i, 'r>, 'b> =
//            let newHead = f |****< (fill, i)
//            let newTail = f |****> (recurse (tail, i))
//            Cons(newHead, newTail)


//let t1 =
//    Provider.op_Explicit (Provide, Resolve, recurse, (Cons(Hole((fun _ -> ())), Nil), 2))

//let t1 =
//    Provider.op_Explicit (Provide, Resolve, recurse, (Cons(Fill(2, (fun _ -> ())), Nil), 2))

let provider = Provider.Provide

let rec inline recurse tail i =
    Provider.op_Explicit (Provide, Resolve, recurse, (tail, i))

let asa () =
    Provider.op_Explicit (Provide, Resolve, recurse, (asList (), A))

let asb () =
    Provider.op_Explicit (Provide, Resolve, recurse, (asList (), B))

let bsa () =
    Provider.op_Explicit (Provide, Resolve, recurse, (bsList (), A))

let bsb () =
    Provider.op_Explicit (Provide, Resolve, recurse, (bsList (), B))

let absa () (*: Cons<Fill<IA, int>, Cons<Hole<IB, string>, Nil>>*) =
    Provider.op_Explicit (Provide, Resolve, recurse, (absList (), A))

let absb () : Cons<Hole<IA, int>, Cons<Fill<IB, string>, Nil>> =
    Provider.op_Explicit (Provide, Resolve, recurse, (absList (), B))
