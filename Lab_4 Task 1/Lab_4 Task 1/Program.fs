open System

type 'T Btree =
    | Node of 'T * 'T Btree * 'T Btree
    | Nil

[<EntryPoint>]
let main args =
    // Функция для инфиксного обхода
    let infix root left right =
        left ()
        root ()
        right ()

    // Итеративный обход дерева с высотой
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

    // Создание отступов для отображения уровня
    let spaces n =
        List.fold (fun s _ -> s + "    ") "" [ 0..n ]

    // Вывод дерева с отображением уровней
    let printTree t =
        printfn "\nСтруктура дерева:"
        printfn "Уровень | Значение"
        printfn "--------+----------"
        iterh infix (fun x h -> printfn "   %d    | %s%O" h (spaces h) x) t

    // Вспомогательные функции для AVL-дерева
    let height tree =
        let rec h t =
            match t with
            | Nil -> 0
            | Node (_, L, R) -> 1 + max (h L) (h R)
        h tree

    let balanceFactor tree =
        match tree with
        | Nil -> 0
        | Node (_, L, R) -> height L - height R

    // Повороты для балансировки
    let rotateRight tree =
        match tree with
        | Node (x, Node (y, yLeft, yRight), right) ->
            Node (y, yLeft, Node (x, yRight, right))
        | _ -> tree

    let rotateLeft tree =
        match tree with
        | Node (x, left, Node (y, yLeft, yRight)) ->
            Node (y, Node (x, left, yLeft), yRight)
        | _ -> tree

    // Балансировка дерева
    let balance tree =
        match tree with
        | Nil -> Nil
        | Node (x, L, R) ->
            let bf = balanceFactor tree
            if bf > 1 then
                if balanceFactor L < 0 then
                    Node (x, rotateLeft L, R) |> rotateRight
                else
                    rotateRight tree
            elif bf < -1 then
                if balanceFactor R > 0 then
                    Node (x, L, rotateRight R) |> rotateLeft
                else
                    rotateLeft tree
            else
                tree

    // Вставка в AVL-дерево с балансировкой
    let rec insertAVL t x =
        match t with
        | Nil -> Node(x, Nil, Nil)
        | Node(z, L, R) ->
            if x < z then
                Node(z, insertAVL L x, R) |> balance
            elif x > z then
                Node(z, L, insertAVL R x) |> balance
            else
                t
        |> balance

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

    // Построение сбалансированного AVL-дерева
    let integerTree = randomValues |> List.fold insertAVL Nil
    printfn "\nСбалансированное AVL-дерево целых чисел:"
    printTree integerTree

    printfn $"\nВысота дерева: {height integerTree}"

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
