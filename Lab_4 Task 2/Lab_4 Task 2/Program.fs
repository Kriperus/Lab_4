open System

type Btree<'T> =
    | Node of 'T * Btree<'T> * Btree<'T>
    | Nil

let infix root left right =
    left ()
    root ()
    right ()

let iterh trav f t =
    let rec tr t h =
        match t with
        | Node (x, L, R) ->
            trav
                (fun () -> f x h)
                (fun () -> tr L (h + 1))
                (fun () -> tr R (h + 1))
        | Nil -> ()
    tr t 0

let spaces n =
    List.fold (fun s _ -> s + "    ") "" [ 0..n ]

let printTree t =
    printfn "\nСтруктура дерева:"
    printfn "Уровень | Значение"
    printfn "--------+----------"
    iterh infix (fun x h -> printfn "   %d    | %s%O" h (spaces h) x) t

let rec insert t x =
    match t with
    | Nil -> Node(x, Nil, Nil)
    | Node(z, L, R) ->
        if x < z then
            Node(z, insert L x, R)
        else
            Node(z, L, insert R x)

let rec treeFold f acc tree =
    match tree with
    | Nil -> acc
    | Node(x, left, right) ->
        let leftAcc = treeFold f acc left
        let currentAcc = f leftAcc x
        treeFold f currentAcc right

let generateRandomStrings count =
    let random = Random()
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    [
        for _ in 1..count do
            let length = random.Next(3, 11)
            let randomString =
                [| for _ in 1..length do
                       yield chars.[random.Next(chars.Length)] |]
                |> System.String
            yield randomString
    ]

[<EntryPoint>]
let main args =
    printfn "Введите количество элементов дерева:"
    let count = Console.ReadLine() |> Convert.ToInt32

    let randomStrings = generateRandomStrings count
    printfn "\nИсходный список случайных строк: %A" randomStrings

    // Построение исходного дерева
    let stringTree = randomStrings |> List.fold insert Nil
    printfn "\nИсходное дерево строк:"
    printTree stringTree

    // Вычисление суммарной длины строк с помощью fold
    let totalLength = 
        treeFold (fun acc str -> acc + String.length str) 0 stringTree
    printfn "\nСуммарная длина всех строк в дереве: %d" totalLength

    0