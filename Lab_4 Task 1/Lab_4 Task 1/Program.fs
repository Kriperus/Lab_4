open System

type 'T Btree =
    | Node of 'T * 'T Btree * 'T Btree
    | Nil

[<EntryPoint>]
let main args =
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

    // Вставка элемента в бинарное дерево поиска
    let rec insert t x =
        match t with
        | Nil -> Node(x, Nil, Nil)
        | Node(z, L, R) ->
            if x < z then
                Node(z, insert L x, R)
            else
                Node(z, L, insert R x)

    // Функция map для дерева
    let rec treeMap f tree =
        match tree with
        | Nil -> Nil
        | Node(x, left, right) ->
            Node(f x, treeMap f left, treeMap f right)

    // Генерация случайного списка целых чисел
    let generateRandomList count =
        let r = Random()
        [ for _ in 1..count do
              yield r.Next(-150, 160) ]

    printfn "Введите количество элементов дерева:"
    let count = Console.ReadLine() |> Convert.ToInt32

    let randomValues = generateRandomList count
    printfn "\nИсходный список целых чисел: %A" randomValues

    // Построение исходного дерева
    let integerTree = randomValues |> List.fold insert Nil
    printfn "\nИсходное дерево целых чисел:"
    printTree integerTree

    printfn "\nВведите делитель для преобразования в вещественные числа:"
    let divisor = Console.ReadLine() |> Convert.ToDouble

    if divisor = 0.0 then
        printfn "\nОшибка: деление на ноль невозможно!"
    else
        let floatTree =
            treeMap (fun x -> Math.Round(float x / divisor, 3)) integerTree

        printfn $"\nДерево вещественных значений (деление на {divisor}):"
        printTree floatTree

    0