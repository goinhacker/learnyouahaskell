# I/O 다루는 함수들

이번 챕터에서는 하스켈에 I/O 작업을 다루는 함수들에 대해서 알아보겠습니다.

## putStr

`putStrLn`과 유사한 함수로 문자열을 받아서 I/O 작업을 리턴합니다. `putStrLn` 함수와의 차이점은 문자열을 출력하고 개행을 넣지않는다는 점 입니다.

```haskell
main = do   putStr "Hey, "  
            putStr "I'm "  
            putStrLn "Andy!"
```

```haskell
$ runhaskell putstr_test.hs
Hey, I'm Andy!
```

함수의 타입은 `putStr :: String -> IO ()`입니다. 따라서 함수 수행 결과는 아무것도 하지않는 I/O 작업에 캡슐화됩니다.

## putChar

하나의 문자를 받아서 I/O 작업을 반환합니다. 받은 문자를 출력하는 함수입니다.

```haskell
main = do   putChar 't'  
            putChar 'e'  
            putChar 'h'
```

```haskell
$ runhaskell putchar_test.hs
teh
```

`putStr` 함수는 내부적으로 `putChar`를 재귀적으로 사용하여 구현되어 있습니다. `putStr`의 재귀 종료 조건은 빈 문자열입니다. 따라서 만약 빈 문자열을 넣으면 `return ()`을 수행하여 아무것도 하지않는 I/O 작업을 리턴합니다. 만약 비어있지 않으면 `putChar`를 사용하여 첫번째 문자를 출력하고, `putStr`을 재귀 호출합니다. 아래 내부 구현 내용 입니다.

```haskell
putStr :: String -> IO ()  
putStr [] = return ()  
putStr (x:xs) = do  
    putChar x  
    putStr xs
```

이 예제를 통해서 순수한 코드만으로 I/O를 어떻게 재귀적으로 사용하는지 알 수 있습니다. 순수한 코드처럼 종료 조건을 정의하고, 종료조건에 해당하지 않을때는 첫번째 문자를 출력하고 나머지 문자열들을 출력합니다.

## print

어떤 타입의 값을 받아서 콘솔에 출력하는 함수입니다. `Show`의 인스턴스로 `show`를 수행합니다. 기본적으로 `putStrLn . show`와 동일합니다. 따라서 `show`를 먼저 수행하고, `putStrLn`에 결과를 넘겨서 최종적으로 I/O 작업을 리턴하여 출력합니다.

```haskell
main = do   print True  
            print 2  
            print "haha"  
            print 3.2  
            print [3,4,3]
```

```haskell
$ runhaskell print_test.hs
True  
2  
"haha"  
3.2  
[3,4,3]
```

이 예제에서는 I/O 작업을 위해서 `main`을 사용하였지만, GHCI를 사용할 수도 있습니다. 사실 엔터키를 누를때 GHCI는 내부적으로 `print`를 호출하여 콘솔에 출력합니다.

```haskell
ghci> 3
3
ghci> print 3
3
ghci> map (++"!") ["hey","ho","woo"]
["hey!","ho!","woo!"]
ghci> print (map (++"!") ["hey","ho","woo"])
["hey!","ho!","woo!"]
```

문자열을 출력할때는 주로 `putStrLn`을 사용하고, 다른 타입인 경우에는 `print`를 사용합니다.

## getChar

입력으로부터 하나의 문자를 읽는 함수입니다. I/O 작업에 `Char`가 포함되기때문에 타입선언은 `getChar :: IO Char`입니다. 내부적으로는 버퍼링을 하고있기때문에 엔터를 누르기전까지는 문자를 읽지않습니다.

```haskell
main = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            main  
        else return ()
```

```haskell
$ runhaskell getchar_test.hs
hello sir  
hello
```

스페이스가 입력되면 출력을 종료하는 프로그램입니다. `hello sir`를 입력하면 `hello`까지만 입력받다가 종료될 것 같지만, 내부적으로 버퍼링하고 있기때문에 엔터를 입력할때까지 입력을 받고, 출력될때 스페이스 이전까지만 출력된 것을 볼 수 있습니다.

## when

`Control.Monad`에 정의된 함수입니다. 마치 switch문과 같은 명령문같이 보이지만, 일반적인 함수입니다. boolean 값과 I/O 작업을 받아서, 만약 `True`이면 동일한 I/O 작업의 결과를 돌려줍니다. `False`일때는 `return ()`을 리턴합니다.

```haskell
import Control.Monad   

main = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        main
```

이전에 `getChar`를 활용한 예제를 `when`을 사용해서 다시 작성했습니다. `if something then do some I/O action else return ()`를 간결하게 작성하였습니다.

## sequence

I/O 작업 목록을 받아서 I/O 작업을 하나씩 수행한 I/O 작업들을 리턴합니다. 따라서 결과는 입력받은 모든 I/O 작업들의 리스트입니다. 함수의 타입은 `sequence :: [IO a] -> IO [a]`입니다.

```haskell
 main = do  
    a <- getLine  
    b <- getLine  
    c <- getLine  
    print [a,b,c]
```

이 작업은 아래와 동일합니다.

```haskell
main = do  
    rs <- sequence [getLine, getLine, getLine]  
    print rs
```

따라서 `sequence [getLine, getLine, getLine]`는 `getLine`을 세번 수행하는 I/O 작업을 만듭니다. 이 결과를 이름에 바인딩하면 모든 수행 결과들의 리스트가 됩니다. 따라서 사용자가 콘솔에 입력한 세가지의 리스트가 출력됩니다.

`sequence`는 map 함수에 `print` 또는 `putStrLn`같은 함수들을 리스트로 적용할때 사용됩니다. `map print [1,2,3,4]`는 I/O 작업을 만들지 못합니다. 이 경우 `[print 1, print 2, print 3, print 4]`처럼 I/O 작업의 리스트를 만들게됩니다. 따라서 I/O 작업안에 I/O 작업들의 리스트로 변경하려면 아래와같이 `sequence`를 사용해야 합니다.

```haskell
ghci> sequence (map print [1,2,3,4,5])
1
2
3
4
5
[(),(),(),(),()]
```

여기서 마지막에 `[(),(),(),(),()]`은 무엇일까요? 이것은 GHCI에서 I/O 작업을 수행하고 출력할때, 결과가 `()`인 경우는 제외하고 출력해서 발생한 출력입니다.

## mapM, mapM\_

위와같이 I/O 작업을 리스트로 리턴하는 mapping 함수는 sequencing을 하는 것이 일반적이라서 `mapM`, `mapM_` 유틸리티 함수가 만들어졌습니다. `mapM`은 함수와 리스트를 받아서, 리스트에 함수들을 적용하고 순서대로 수행합니다. `mapM_`도 동일하나 I/O 작업 결과를 버린다는 점만 다릅니다. 일반적으로는 I/O 작업의 순서를 필요없기때문에 `mapM_`를 사용합니다.

```haskell
ghci> mapM print [1,2,3]
1
2
3
[(),(),()]
ghci> mapM_ print [1,2,3]
1
2
3
```

## forever

I/O 작업을 받아서 해당 I/O 작업을 무한히 반복하는 I/O 작업을 리턴합니다. `Control.Monad`에 정의되어 있습니다. 아래 예제를 보겠습니다.

```haskell
import Control.Monad  
import Data.Char  

main = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l
```

사용자의 입력을 무한히 받아서 출력하는 프로그램입니다.

## forM

`Control.Monad`에 정의된 함수로 `mapM`과 유사합니다. 단지 두 입력 매개변수만 위치가 바뀌었습니다. 첫번째 매개변수로 리스트를 받고, 두번째 매개변수로 리스트에 맵핑할 함수를 받습니다. 이 함수는 아래와 예제와같이 사용합니다.

```haskell
import Control.Monad  

main = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors
```

`(\a -> do ... )` 부분은 숫자 a를 받아서 I/O 작업을 리턴하는 함수입니다. 이때는 반드시 함수 전체를 괄호로 묶어야 합니다. 그렇지 않으면 람다는 마지막 두 I/O 작업도 속한다고 생각합니다. _do_ 블럭안에서 `return color`를 수행하였습니다. 여기서 `forM` 함수는 I/O 작업을 생성하고, 그 결과를 `colors`에 바인딩 합니다. `colors`는 문자열의 리스트입니다. 마지막으로 `mapM putStrLn colors`로 모든 색상들을 화면에 출력합니다.\(`mapM putStrLn colors` 대신 `forM colors putStrLn`을 사용해도 됩니다.\)

`forM` 함수는 리스트내의 모든 값들의 I/O 작업을 만든다고 생각할 수 있습니다. 각 I/O 작업은 사용된 값에따라 달라집니다. 최종적으로는 액션들을 수행하고 결과들은 어떤 것에 바인딩합니다.

```haskell
$ form_test.hs
Which color do you associate with the number 1?  
white  
Which color do you associate with the number 2?  
blue  
Which color do you associate with the number 3?  
red  
Which color do you associate with the number 4?  
orange  
The colors that you associate with 1, 2, 3 and 4 are:  
white  
blue  
red  
orange
```

위와같은 작업은 꼭 `forM` 함수가 아니어도 할 수 있지만, 좀 더 가독성이 좋습니다. 일반적으로 _do_를 사용한 지점에 몇몇 작업들을 맵핑하고 순서를 지정할때 `forM`을 사용합니다.

이번 챕터에서는 입력과 출력의 기본을 학습했습니다. 또한 I/O 작업의 정의와 실제 입출력할때 어떤게 활용되는지 알았습니다. 하스켈에서 I/O 작업은 다른 값들과 유사합니다. I/O 작업도 어떤 함수의 매개변수로 넘길 수 있고, 결과로서 I/O 작업을 리턴할 수 도 있습니다. 특이한 점은 `main` 함수안에서만 실행된다는 것입니다. 각 I/O 작업은 또한 어떤 결과를 캡슐화하여 담을 수 있습니다.

