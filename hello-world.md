# 입출력 Hello World

하스켈은 순수한 함수형 언어입니다. 명령형 언어에서는 실행하기위한 단계 절차를 주는 반면에 함수형 언어는 어떤 일을 하는지 정의해야 합니다. 하스켈에서 함수는 변수의 내용을 바꾸는 것과 같이 어떤 상태를 바꿀 수 없습니다\(상태가 변화하는 함수는 side-effect를 가지고 있다고 함\). 함수는 주어진 입력을 기반으로 어떤 결과를 돌려주는 역할만 할 뿐입니다. 만약 동일한 파라메터로 2번 호출된다면 반드시 같은 결과를 돌려줄 것입니다. 명령형 프로그래밍에 익숙한 프로그래머는 함수의 활용을 제한하는 것으로 느낄 수 있지만, 함수의 상태를 바꾸지않는 것이 실제로 어떻게 좋은 것인지 지금까지 살펴보았습니다. 명령형 프로그래밍에서는 주어진 입력에 대해서 내부적으로 어떤 상태 변화가 있을지 알 수 없습니다. 예를들어 함수형 프로그래밍에서는 이진 검색 트리에 새로운 노드를 넣을때도 기존 트리의 상태를 변경하지 않습니다. 즉, 새로운 노드가 삽입될때 기존 트리를 변형하지 않고, 새로운 트리를 만들어서 리턴합니다.

함수가 상태를 변경할 수 없는 것이 좋은 이유는 알았지만, 한가지 문제가 있습니다. 만약 함수가 어떤 것도 바꿀 수 없다면, 함수에 의해서 계산된 것이 무엇인지 어떻게 알려줄 수 있을까요? 계산 결과를 알려주기 위해서는 출력 디바이스를 변경해야 합니다\(보통 출력 방향을 스크린으로 변경\).

하스켈은 프로그램의 순수한 부분과 상태를 변경해야하는 순수하지 못한 부분\(side-effect가 존재하는 부분\)을 깔끔하게 분리해서 처리하는 매우 똑똑한 시스템을 가지고 있습니다. 분리된 두 부분으로 게으른 평가\(laziness\), 견고성\(robustness\), 모듈성과 같은 순수한 함수형 프로그래밍이 제공하는 모든 장점을 활용하여 외부 컴포넌트와 커뮤니케이션할 수 있습니다.

## Hello, world 프로그램

지금까지는 테스트를 위해서 GHCI내에서 함수를 로딩하고 실행하였습니다. 이제부터는 실제 하스켈 프로그램을 만들어 실행할 것 입니다. 이번 챕터에서는 "hello, world" 프로그램을 만들어 보겠습니다.

> 여기서부터 작성하는 프로그램은 선호하는 Editor 사용해도 되지만, 프로그램을 실행하기 위해서 Linux-like 환경을 가정합니다. 윈도우 환경이라면 Cygwin을 설치해서 사용하거나, Windows 10의 Linux Shell을 활용할 것은 권장합니다.

```haskell
main = putStrLn "hello, world"
```

위와같이 작성하고 `helloworld.hs` 파일로 저장합니다.

터미널에서 `helloworld.hs` 파일이 위치한 디렉토리로 이동한 후, 위와같이 실행하여 컴파일합니다.

```haskell
$ ./helloworld
hello, world
```

컴파일이 완료되면 `./helloworld`로 실행할 수 있습니다.

이제 helloworld 예제에서 사용된 `putStrLn` 함수에 대해서 살펴보겠습니다.

```haskell
ghci> :t putStrLn
putStrLn :: String -> IO ()
ghci> :t putStrLn "hello, world"
putStrLn "hello, world" :: IO ()
```

`putStrLn`은 문자열을 입력받아서 결과 타입에 `()`를 포함한 _I/O 작업_을 리턴한다는 것을 알 수 있습니다\(비어있는 튜플 `()`은 unit이라고 함\). I/O 작업은 실행될때, 어떤 side-effect\(입력에서 읽거나, 화면에 출력하는 것\)를 수행하고 내부에 어떤 리턴 값을 포함합니다. 문자열을 터미널에 출력하는 것은 실제로 어떤 의미있는 리턴값을 가지고 있지 않기때문에, 더미 값으로 `()`가 사용됩니다.

> 빈 튜플은 `()`의 값이고 또한 `()`을 타입으로 가집니다.

I/O 작업은 `main`이라는 이름으로, 프로그램을 실행해야 수행됩니다. 이것은 마치 프로그램 전체에서 I/O 작업을 하나로 제한하는 것처럼 보입니다. 그래서 _do 구문을 사용하여 여러 I/O 작업들을 하나로 결합할 수 있습니다._

```haskell
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
```

마치 명령형 프로그램처럼 보입니다. _do_를 사용한 이후에는 명령형 프로그램처럼 수행해야하는 단계별로 작성하였습니다. 그리고 각 I/O 작업들은 _do_ 문법에 의해서 하나의 I/O 작업으로 묶입니다. 하나로 묶인 I/O 작업의 타입은 마지막 I/O 작업의 타입인 `IO ()`가 됩니다.

이런 이유로 `main`의 타입은 항상 `main :: IO something`이 됩니다. 여기서 `something`은 어떤 구체적인 타입입니다. 관례적으로 `main`에 대해서는 타입 선언을 별도로 명시하지 않습니다.

여기서 `name <- getLine`은 입력 라인을 읽어서 `name` 변수에 저장하는 것처럼 보입니다. 정말 그런지 확인해 보겠습니다.

```haskell
ghci> :t getLine
getLine :: IO String
```

`getLine`의 타입을 보면 I/O 작업이고 `String` 결과 타입을 포함하고 있습니다. 이것은 사용자가 터미널에서 무언가를 입력할때까지 기다렸다가 문자열로 표시될 것이기 때문입니다. 그렇다면 `name <- getLine` 다음에는 무슨 일이 일어날까요? `name <- getLine` 코드를 그대로 읽으면 _getLine I/O 작업을 수행하고 결과 값을 name에 바인딩한다_입니다. `getLine`의 타입은 `IO String`이므로 `name`의 타입은 `String`일 것입니다.

I/O 작업은 다른 세상에서 어떤 작업을 수행하고 데이터를 가져올 수 있는 다리가 있는 상자에 비유할 수 있습니다. 한번 데이터를 가져오면 상자를 열고 그 안에있는 데이터를 가져오는 유일한 방법은 `<-`를 사용하는 것 입니다. I/O 작업에서 데이터를 가져오는 경우 다른 I/O 작업을 수행할 때만 데이터를 가져올 수 있습니다. 이것이 하스켈에서 순수한 코드와 그렇지 않은 코드를 깔끔하게 분리하는 방법입니다.

`getLine`은 결과 값이 두번 수행될때 동일한 결과를 보장하지 않으므로 순수하지 못합니다. 이것이 `getLine` 함수가 I/O 타입 생성자로 오염된\(tainted\) 이유이고, I/O 코드에서만 해당 데이터를 가져올 수 있습니다. 또한 I/O 코드는 오염되어 있기 때문에 오염된 I/O 데이터에 의존하는 계산은 순수하지 못한 결과를 가져옵니다.

여기서 오염된\(tainted\)것은 순수한 코드에서 I/O 작업에 포함된 결과를 절대로 사용하지 못한다는 것을 의미하진 않습니다. I/O 작업을 `name`에 바인딩할 때, 일시적으로 I/O 작업 내의 데이터를 오염시키지 않습니다\(un-taint\). `name <- getLine`을 실행할때, I/O 작업내에서 `name`는 그냥 일반적인 문자열이기 때문입니다.

```haskell
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Read this carefully, because this is your future: " ++ tellFortune name
```

매개변수로 이름을 받아서 이름에 따른 운명과 생애를 알려주는 함수입니다. 여기서 `tellFortune`\(또는 `name`을 인자로 받는 어떤 함수\)는 I/O에 대해서 어떤 것도 알 필요가 없습니다. 일반적인 `String -> String` 함수입니다.

```haskell
nameTag = "Hello, my name is " ++ getLine
```

이 코드는 문제가 없을까요? `++` 함수는 동일한 타입의 리스트 두개를 파라메터로 받아야 하기 때문에 동작을 하지 않습니다. 왼쪽 파라메터는 `String` 타입이지만, `getLine`의 타입은 `IO String`입니다. 문자열과 I/O 작업은 연결할 수 없습니다. 먼저 String 타입의 값을 얻기 위해 I/O 작업의 결과를 가져와야 합니다. 이렇게 하려면 다른 I/O 작업의 내부에서 `name <- getLine`와 같은 것을 해야합니다. 순수하지 못한 데이터를 다루려면, 순수하지 못한 환경에서 해야만 합니다. 순수하지 못한 데이터를 다루기 시작하면, 순수하지 못한 코드가 여기저기 퍼져나가게 됩니다. 따라서 이런 순수하지 못한 코드를 작성하는 것을 최소화 해야합니다.

모든 I/O 작업에는 캡슐화된 결과가 있습니다. 이것을 이용하면 이전에 다루었던 이름을 묻는 함수는 아래와 같이 작성될 수 있습니다.

```haskell
main = do
    foo <- putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
```

하지만 foo는 `()`의 값을 할당하는데 의미가 없습니다. 여기서 마지막 `putStrLn` 은 어떤 값에도 바인딩하지 않았습니다. 이것은 _**do**_ **블록에서 마지막 작업은 어떤 이름에도 바인딩할 수 없기 때문입니다.** 그 이유에 대해서는 모나드를 학습할때 정확히 알게 될 것이고, 여기서는 _do_ 블록이 자동으로 마지막 작업에서 값을 추출하여 자신의 결과에 바인딩하는 것으로 이해하고 넘어가겠습니다.

_do_블록에서는 마지막 라인을 제외한 모든 라인은 바인딩이 가능합니다. 따라서 `putStrLn "BLAH"`는 `_ <- putStrLn "BLAH"`로 작성될 수 있습니다. 하지만 역시 이렇게 작성하는 것은 의미가 없습니다. 따라서 의미있는 결과를 포함하지 않을때는 `<-`는 생략하고 `putStrLn something`와 같이 작성합니다.

```haskell
name = getLine
```

지금까지의 설명을 이해했다면 이 예제가 입력을 읽어서 결과를 `name`에 바인딩하는 것이 아니라는 것을 알 수 있습니다. I/O 작업에서는 반드시 `<-`를 사용해서 순수하지 못한 작업이 분리되어야 합니다.

한개의 I/O 작업은 `main` 함수에서 수행될 수 있고, 여러개의 I/O 작업을 합성하는 것은 _do_ 블록내에서 가능합니다. 그리고 아래 예제와 같이 GHCI에서 바로 결과를 리턴받을때도 I/O 작업을 쓸 수 있습니다.

```haskell
ghci> putStrLn "HEEY"
HEEY
```

이전 챕터에서 배웠던 _let_ 바인딩은 `let bindings in expression`의 형태를 가집니다. 여기서 `bindings`는 표현식이 바인딩되는 이름이고, `expression`은 평가되는 부분입니다. 리스트 정의\(list comprehesion\)에서는 _in_을 사용하지 않아도 됐었는데, _do_ 블럭에서도 _in_을 사용하지 않고, _let_을 사용할 수 있습니다.

```haskell
import Data.Char

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
```

이 예제에서 _let_이 _do_ 블록안에서 I/O 작업과 함께 어떻게 사용되는지 알 수 있습니다. 입력받은 값에 `map toUpper firstName`을 적용하여 대문자로 변환하였습니다. 그리고 마지막에 _let_을 통해서 바인딩한 bigFirstName을 문자열 표현에 사용하였습니다.

그렇다면 언제 `<-`를 사용하고, 언제 _let_을 사용할까요? `<-`**는 I/O 작업을 수행하고, 그 결과를 이름에 바인딩하는 것입니다.** 하지만 `map toUpper firstName`은 I/O 작업이 아닙니다. 하스켈의 순수한 표현식 입니다. 따라서 _let_은 이런 순수한 작업을 이름에 할당할때 사용됩니다. `let firstName = getLine`과 같이 사용될 수는 없습니다. getLine은 I/O 작업이기 때문에 `<-`가 사용되어야 합니다.

아래 예제는 한라인씩 계속 읽어서 해당 라인의 단어들이 뒤짚혀서 찍히는 함수입니다. 그리고 빈 라인을 만나면 실행이 중단될 것 입니다.

```haskell
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
```

> 참고: 위 예제를 실행하기 위해서 `ghc --make helloworld` -&gt; `./helloworld`하거나, `runhaskell helloworld.hs`와 같이 `runhaskell` 명령을 사용할수도 있다.

`reverseWords` 함수는 1. "hey there man"와 같은 문자열을 받아서 2. `words`를 호출해서 `["Hey", "there", "man"]`으로 만들고,  
3. `reverse` 함수로 변환해서 `["yeh ereht nam"]`로 만듭니다. 4. 이렇게 변환된 리스트를 `unwords` 함수에 넣으면 "yeh ereht nam"이 됩니다.

여기서는 함수 함성을 사용해서 표현했는데, 만약 함수 함성을 사용하지 않으면 `reverseWords st = unwords (map reverse (words st))`와 같이 표현됩니다.

`main` 함수를 살펴보자. 1. 먼저 `getLine` 함수를 호출해서 값을 입력받고 `line`에 할당한다. 2. 만약 line이 null이면 어떤 I/O 작업을 수행하고 끝나고, 3. 그렇지 않으면 I/O 작업은 else에서 수행된다. 그리고 else에서는 다시 _do_ 블록이 시작된다.

_if_문에서 I/O 작업을 할때는 `if condition then I/O action else I/O action`과 같이 if와 else가 모두 I/O 작업이어야 한다.

> 하스켈에서는 if문도 expression이기 때문에 어떤 값을 가지기 위해서 반드시 else를 정의해야한다.

else문에서도 반드시 하나의 I/O 작업이 수행되어야 하는데, 두개 이상의 I/O 작업을 사용하기 위해서 _do_ 블록으로 묶었다. else문의 _do_ 블록 안에서는 `reverseWords` 함수에 `getLine`을 통해서 얻은 값을 넣어서 터미널에 출력한다. 그리고 마지막으로 다음 라인을 받기위해서 `main`을 재귀호출한다.

이제 `null line`이 _true_였을때, `then return ()`를 살펴보자. C나 자바와 같은 명령형 언어에 익숙한 사람이라면 `return`을 보고 무심코 지나쳤을 수 있습니다. 하지만 **하스켈에서** `return`**은 다른 언어의** `return`**과는 전혀 다릅니다.** 명령형 언어에서 `return`은 보통 메서드 또는 서브루틴의 실행을 종료하고 호출자에게 값을 돌려줍니다. 하스켈에서 `return`은 I/O 작업을 순수한 값밖으로 뺍니다. 또다시 상자 비유하면 `return`을 통해서 값을 가져와서 상자에 포장하는 것과 같습니다. I/O 작업은 실제로 아무것도 하지않고, 결과로 캡슐화된 값을 갖게됩니다. 따라서 I/O 상황에서 `return "haha"`는 `I/O String` 타입을 가지게 될 것 입니다. 여기서 순수한 값을 아무것도 하지않는 I/O 작업으로 변환하는 이유는 빈 라인을 가져오기 위한 I/O 작업이 한개가 더 필요했기 때문입니다.

아래 예제에서 `return`을 사용하는 것은 사용하는 것은 _do_ 블럭의 I/O 작업을 실행하거나 끝내지 않습니다.

```haskell
main = do  
    return ()  
    return "HAHAHA"  
    line <- getLine  
    return "BLAH BLAH BLAH"  
    return 4  
    putStrLn line
```

이 프로그램은 중간에 끝나지않고 마지막 라인까지 진행됩니다. 각 `return`은 어떤 I/O 작업도 일어나지않고, 단순히 결과를 캡슐화해서 버리기만 합니다. 만약 결과를 사용하려면 아래 예제와 같이 바인딩을 해야합니다.

```haskell
main = do  
    a <- return "hell"  
    b <- return "yeah!"  
    putStrLn $ a ++ " " ++ b
```

결과적으로 `return`의 의미는 `<-`과 반대의 개념입니다. `return`은 값을 가져와서 박스에 포장하는 것이고, `<-`은 박스를 가져와서 값을 밖으로 꺼내는 것 입니다. 하지만 아래 예제와 같이 `let`을 활용하면 `return`을 사용할 필요가 없습니다.

```haskell
main = do  
    let a = "hell"  
        b = "yeah"  
    putStrLn $ a ++ " " ++ b
```

`return`은 아래와 같은 상황에서 주로 활용합니다.

* 아무것도 하지않는 I/O 작업을 만들때
* I/O 작업이 do 블럭의 마지막 작업의 결과값을 가지는 것을 원하지 않을때
* 항상 원하는 결과를 포함하는 I/O 작업을 만들때

> do 블럭은 하나의 I/O 작업만 가질 수 있습니다. `then do return ()` 방식으로 작성하면 `else`도 do 블럭을 가지기 때문에, 이와 같은 방식을 선호하는 사람도 있습니다.

