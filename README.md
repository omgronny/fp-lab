# Лабораторная работа 1. Проект Эйлера

Вариант: 11, 20

Цель: освоить базовые приёмы и абстракции функционального программирования: функции, поток управления и поток данных, сопоставление с образцом, рекурсия, свёртка, отображение, работа с функциями как с данными, списки.

## Условие

### Задача 11:
What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the grid?

### Задача 20:

`n!` means `n × (n - 1) x • • • × 3 x 2 x 1.`
For example, `10! = 10 x 9 x • . . × 3 x 2 x 1 = 3628800`
and the sum of the digits in the number `10!` is `3 + 6 + 2+8+8+0+0 = 27.`
Find the sum of the digits in the number `100!`.

## Реализация

Полный код различных реализаций представлен в `src`. Ниже продемонстрированы куски, явно использующие необходимые подходы

### Различные реализации задания 20

- Рекурсия:

```haskell
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

- Хвостовая рекурсия:

```haskell
factorial :: Integer -> Integer
factorial = tailFactorial 1
    where
        tailFactorial acc n'
            | n' == 0    = acc
            | otherwise = tailFactorial (acc * n') (n' - 1)
```

- map + явное разделение на генерацию и свертку

```haskell
digitSum :: Integer -> Int
digitSum n = iterateThrough (show n)
    where
        stringToNumbers = map (\x -> ord x - 48)
        iterateThrough str = foldr (+) 0 (stringToNumbers str)
```

*Реализация факториала осталась прежней*

- Реализация с использованием бесконечных списков

```haskell
factorial :: Int -> Integer
factorial 0 = 1
factorial n = product (take n [1..])
```

### Различные реализации задания 11

- Рекурсия:

```haskell
getRows :: [a] -> [[a]]
getRows [] = []
getRows row = take 4 row : getRows (tail row)
```

- Хвостовая рекурсия:

```haskell
productRow :: [Int] -> [Int]
productRow row = productRowTail row []
  where
     productRowTail :: [Int] -> [Int] -> [Int]
     productRowTail [] acc = acc
     productRowTail row acc = productRowTail (tail row) (product (take 4 row) : acc)

horizontalProducts :: [[Int]] -> [Int]
horizontalProducts grid = horizontalProductsTail grid []
  where
    horizontalProductsTail :: [[Int]] -> [Int] -> [Int]
    horizontalProductsTail [] acc = acc
    horizontalProductsTail (g:grid) acc = horizontalProductsTail grid (productRow g ++ acc)
```

*Для примера представлен обход поля и собирание произведений по горизонтали*

- map + явное разделение на генерацию и свертку

```haskell
getRows :: [a] -> [[a]]
getRows [] = []
getRows row = take 4 row : getRows (tail row)


horizontalRows :: [[Int]] -> [[Int]]
horizontalRows = concatMap getRows
```

```haskell
doMain :: Int
doMain = let grid = Grid.getGrid in
    maximum . getProducts $
        horizontalRows grid ++
        verticalRows grid ++
        createDiagonalListFrom grid ++
        createDiagonalListFrom (map reverse grid)
```

- Реализация с использованием бесконечных списков

```haskell
doCreateDiagonalListFrom :: [[Int]] -> [[Int]]
doCreateDiagonalListFrom = verticalRows . shiftRows
    where
        shiftRows :: [[Int]] -> [[Int]]
        shiftRows = zipWith drop [0..]

createDiagonalListFrom :: [[Int]] -> [[Int]]
createDiagonalListFrom [] = []
createDiagonalListFrom grid = doCreateDiagonalListFrom grid ++ createDiagonalListFrom (tail grid)
```

### Вывод

Функциональный подход интересный своей математичностью. Иногда эта самая математичность выливается в очень красивые и изящные решения, а иногда дает довольно странный и не очень естественный код 

При этом мне кажется, что написанный так код не отличается своей читабельностью и из-за этого его тяжелее поддерживать (я вчера написал код, а пока вставлял его в отчет поратил 7 минут, чтобы снова его понять)

Ну и помимо этого такой подход плохо ложится на модель железа, за счет чего не получается делать хорошие оптимизации



