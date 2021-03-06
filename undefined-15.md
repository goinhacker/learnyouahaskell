# 예외

모든 언어는 실패할수있는 프로시져, 함수 등의 코드를 가지고 있습니다. 그리고 각 언어들은 다른 방법으로 실패에 대한 핸들링을 합니다. C언어의 경우, -1, null가 같은 값을 반환함으로써 함수가 실패했다는 것을 알려줍니다. 반대로 Java나 C\#은 예외를 사용하여 실패를 핸들링합니다. 예외가 발생하면 제어의 흐름이 우리가 에러 핸들링을 위해서 정의한 위치로 바뀌게 됩니다. 여기서 예외에 대한 핸들링을 수행합니다.

하스켈은 `Maybe`나 `Either`와같은 대수적 타입을 사용해서 결과값의 존재 유무를 판단할 수 있습니다. C언어에서 -1을 실패의 의미로 사용합니다. 근데 이것은 사람들끼리의 약속일 뿐이고, 코드를 작성할때 주의하지 않으면 -1값을 정상값처럼 취급할 수도 있습니다. 하스켈의 타입 시스템은 이런 측면에서 더 안전하게 핸들링할 수 있도록 합니다. `a -> Maybe b`는 `Just`에 랩핑된 `b`이거나 `Nothing`을 반환합니다.

하스켈은 실패한 계산을 표현하는 타입을 가지고 있음에도 불구하고, I/O 문맥에서 더 이해하기 쉽기때문에 예외도 제공합니다. 예를들어 파일을 여는 것과 같이 외부의 어떤 것을 다룰때는 어떤 동작을 할지 신뢰할 수 없습니다. 파일이 락에 걸려있을수도 있고, 파일이 없을수도 있습니다. 따라서 이러한 오류가 발생할 경우, 오류를 핸들링하기 위한 부분으로 제어흐름을 이동시키는 것이 좋습니다.

I/O 코드\(순수하지 못한 코드\)는 예외를 발생시킬수 있습니다. 그렇다면 순수한 코드에서는 어떨까요? 순수한 코드에서도 예외가 발생할 수 있습니다. `div`, `head` 함수의 타입은 각각 `(Integral a) => a -> a -> a`와 `[a] -> a` 입니다. `Maybe`나 `Either`를 반환하지 않습니다. 하지만 이 함수들을 실패할 수 있습니다. `div` 함수는 0으로 나누면 실패하고, `head` 함수는 리스트가 비어있을때 실패할 것 입니다.

```haskell
ghci> 4 `div` 0
*** Exception: divide by zero
ghci> head []
*** Exception: Prelude.head: empty list
```

위와같이 **순수한 코드도 예외를 발생시킬수있다. 그러나 I/O 영역에서만 catch할 수 있다.** 즉, `main` 함수내의 _do_ 블럭 안에서 catch할 수 있다. **I/O 영역에서만 예외를 잡을 수 있는 이유는 순수한 코드는 어떤 값이 평가되는 시점을 알 수가 없기때문이다.** 순수한 코드는 lazy한 특성때문에 I/O 코드와는 달리 정의된 실행 순서가 없고, 따라서 예외를 잡을 수 없다.

이전에 프로그램에서 가능한 I/O 부분에서 적은 시간을 보내야 한다고 했었습니다. 순수한 함수는 그 결과가 입력 매개변수에만 의존하기 때문에, 프로그램의 로직은 대부분 순수한 함수안에 있어야 합니다. 순수한 함수는 다른 일을 할 수 없기때문에 그 함수가 반환하는 것만 생각하면 됩니다. I/O에도 약간의 로직\(파일열기 등\)이 필요하긴 하지만 최소한의 유지해야 합니다. 순수한 함수는 기본적으로 게으르고, 언제 평가될지 알 수도 없고, 그게 중요하지도 않습니다. 그러나 순수한 함수가 예외를 던질때는, 평가되는 시점이 중요합니다. 그래서 순수한 함수로부터 발생한 예외는 I/O 부분에서만 catch할 수 있습니다. 그런데 여기서 모순이 생깁니다. 가능한 I/O 부분을 줄여야하지만 예외는 I/O 부분에서만 잡을 수 있습니다. 그렇다고 I/O 부분을 줄이기 위해서 예외를 잡지않으면 프로그램이 죽을 것 입니다. 이에대한 해결방법은 **예외와 순수한 코드를 섞지 않는 것 입니다.** 하스켈의 타입시스템에서 제공하는 `Either`와 `Maybe`를 사용하여 실패할 수 있는 결과를 표현합니다.

여기서는 I/O 예외를 사용하는 방법에 대해서 알아볼 것 입니다. I/O 예외는 `main`의 부분인 I/O 작업안에서 외부와 커뮤니케이션하는 동안 무언가 잘못되면 발생하는 예외입니다. 예를들어 파일을 열고나서야 파일이 삭제되었는지 있는지를 판단할 수 있습니다. 아래 프로그램을 보자.

```haskell
import System.Environment  
import System.IO  

main = do (fileName:_) <- getArgs  
          contents <- readFile fileName  
          putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
```

커맨드라인에서 입력받은 이름을 가진 파일을 열고, 파일에 라인수를 출력하는 프로그램입니다. `getArgs`의 첫번째 문자열만 `fileName`에 바인딩하고, 파일을 읽어서 `contents`에 바인딩 합니다. 마지막으로 `lines` 함수에 `contents`를 넣어서 라인단위로 분리하고, 분리된 리스트의 길이를 계산합니다. 여기서 만약 존재하지않는 파일명을 입력으로 준다면 어떻게 될까요?

```haskell
$ runhaskell linecount.hs i_dont_exist.txt
linecount.hs: i_dont_exist.txt: openFile: does not exist (No such file or directory)
```

에러가 발생했고, 파일이 존재하지 않는다는 내용을 출력했습니다. 이 프로그램은 크래쉬가 발생하여 종료되었는데, 파일이 없으면 적당한 메시지를 출력하도록 하겠습니다. 예외처리의 한가지 방법은 파일을 열기전에 파일의 존재 여부를 확인하는 것 입니다. `System.Directory`의 `doesFileExist` 함수를 사용하면 파일의 존재 여부를 체크할 수 있습니다.

```haskell
import System.Environment  
import System.IO  
import System.Directory  

main = do (fileName:_) <- getArgs  
          fileExists <- doesFileExist fileName  
          if fileExists  
              then do contents <- readFile fileName  
                      putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
              else do putStrLn "The file doesn't exist!"
```

`doesFileExist` 함수의 타입은 `doesFileExist :: FilePath -> IO Bool` 입니다. 따라서 `fileExists <- doesFileExist fileName`와 같이 실행하였습니다. 이 함수는 파일의 존재여부를 알려주는 `Bool`를 가진 I/O 작업을 반환합니다. 그리고나서 if else 구문을 사용하여 적절한 메시지를 출력하였습니다.

또 다른 방법으로 예외\(Exception\)를 사용할 수 있습니다. 파일이 없어서 발생되는 예외는 I/O에서 발생하는 예외이므로 I/O에서 잡는 것이 좋습니다.

예외를 처리하기 위해서는 `System.IO.Error`의 `catch` 함수를 사용합니다. 이 함수의 타입은 `catch :: IO a -> (IOError -> IO a) -> IO a` 입니다. 첫번째 매개변수는 IO 작업입니다. 예를들면 파일을 여는 IO 작업을 첫번째 매개변수로 받습니다. 두번째 매개변수는 첫번째 I/O 작업에서 보낸 예외\(`IOError`\)를 받아서 처리하고 IO 작업을 반환하는 핸들러 함수입니다. 그리고 `catch` 함수는 IO 작업을 반환하는데, 만약 예외가 발생하지 않았다면 첫번째 매개변수로 받았던 I/O 작업을 반환하고, 만약 예외가 발생했다면 핸들러에서 반환한 I/O 작업을 반환합니다.

하스켈의 `catch` 함수는 자바나 파이썬의 try-catch문과 유사합니다. 첫번째 매개변수로 받은 I/O 작업이 try 블록안의 어떤 동작이고, 두번째 매개변수로 받는 예외를 받는 핸들러가 동일하게 예외를 입력으로 받는 catch 블록이라고 할 수 있습니다. try-catch문과 동일하게 예외가 발생하면 핸들러 함수가 호출됩니다.

핸들러 함수는 `IOError` 타입의 값을 받습니다. `IOError`는 I/O 예외가 발생한 것을 나타내는 값으로 발생한 예외의 타입에 관한 정보를 포함합니다. 한가지 문제점은 `IOError`는 구현로직에 의존성을 가지고 있기때문에 패턴매칭을 할 수 없습니다. 즉, 핸들러 내에서 `IO something`으로 패턴매칭할 수 없습니다. 따라서 `IOError` 타입의 값을 알아낼 수 있는 방법에 대해서 알아보겠습니다.

위에서 작성한 프로그램을 `catch` 함수를 사용해서 재작성하였습니다.

```haskell
import System.Environment  
import System.IO  
import System.IO.Error  

main = toTry `catch` handler  

toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
           contents <- readFile fileName  
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  

handler :: IOError -> IO ()  
handler e = putStrLn "Whoops, had some trouble!"
```

이 예제에서는 `catch` 함수의 입력 매개변수 두개를 infix로 받기위해서 backtick\(`)을 사용하였습니다. 이렇게 사용한 이유는 가독성을 높이기 위함입니다. 결과적으로`toTry `catch` handler`는`catch toTry handler`와 동일합니다.`toTry`함수는 I/O 작업을 시도할 작업 내용을 가지고 있고,`handler`함수는`IOError\` 예외에 대한 처리 작업을 포함하고 있습니다. 이 프로그램을 아래와 같이 동작합니다.

```haskell
$ runhaskell count_lines.hs i_exist.txt
The file has 3 lines!
$ runhaskell count_lines.hs i_dont_exist.txt
Whoops, had some trouble!
```

`handler` 함수에서는 `IOError`가 어떤 종류인지를 체크하지 않았습니다. 예외가 발생하면 어떤 종류의 예외든 그냥 `"Whoops, had some trouble!"`를 출력합니다. 이와같은 방식으로 예외가 어떤 타입이든 한가지로 예외처리하는 것은 매우 좋지않은 패턴입니다\(하스켈뿐만 아니라 대부분의 다른 언어에서도 마찬가지입니다.\). 그럼 이제 어떤 예외가 발생했는지 구분하여 처리하도록 하겠습니다. 만약 예상했던 예외이면 적절한 예외처리 작업을 수행하고, 그렇지않으면 받은 예외를 그대로 다시 발생시키도록 하겠습니다.

```haskell
import System.Environment  
import System.IO  
import System.IO.Error  

main = toTry `catch` handler  

toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
           contents <- readFile fileName  
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  

handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | otherwise = ioError e
```

`handler` 함수의 예외처리 부분만 수정되었습니다. 여기서는 `System.IO.Error`의 함수, `isDoesNotExistError`, `ioError`를 사용하였습니다. `isDoesNotExistError`의 타입은 `isDoesNotExistError :: IOError -> Bool`입니다. 이 함수는 `IOError`를 받아서 파일이 존재하지않아서 발생한 에러인지 확인해줍니다. 여기서는 가드\([guard](http://learnyouahaskell.com/syntax-in-functions#guards-guards)\)를 사용했지만, _if else_를 사용할수도 있습니다. 만약 파일이 존재하지않아서 발생한 에러가 아니면, `ioError` 함수를 사용해서 받은 예외를 그대로 재발생\(re-throw\) 시킵니다. `ioError` 함수의 타입은 `ioError :: IOException -> IO a`이고, `IOError`를 받아서 어떤 I/O 작업을 만들어서 반환\(throw\)합니다. 이 I/O 작업의 타입은 실제로 실제로 어떤 결과를 만들지 않기 때문에, 반환하는 타입은 `IO anything`이 될 수 있습니다.

`toTry` 함수의 _do_ 블록내에서 파일이 존재 여부와 관계없은 IO 작업에서 예외가 발생하면, `toTry`catch`handler`에서 `handler` 함수로 넘어가서 재발생\(re-throw\)됩니다.

`IOError`의 값을 확인하기위한 함수들의 종류는 아래와 같습니다.

* `isAlreadyExistsError`
* `isDoesNotExistError`
* `isAlreadyInUseError`
* `isFullError`
* `isEOFError`
* `isIllegalOperation`
* `isPermissionError`
* `isUserError`

대부분의 함수들의 기능은 함수명을 통해서 쉽게 판단이 가능합니다. 여기서 `isUserError` 함수는 프로그램에서 어떤 메시지를 포함한 예외를 만들기위해서 `userError` 함수를 사용한 경우, 이것을 확인하기 위해서 사용합니다. 예를들어서 `ioError $ userError "remote computer unplugged!"`와 같이 하면 `isUserError`를 사용해서 확인할 수 있습니다. \(일반적으로 `userError`를 사용해서 예외를 만들는것보다는 `Either`와 `Maybe`를 사용하는 것이 더 좋다.\)

위에서 소개한 함수들을 사용하면 아래와같이 예외처리를 하게됩니다.

```haskell
handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | isFullError e = freeSomeSpace  
    | isIllegalOperation e = notifyCops  
    | otherwise = ioError e
```

여기서 `freeSomeSpace`와 `notifyCops` 함수는 사용자가 정의한 I/O 작업이라고 가정한다. 예상하지 못한 예외가 발생했을때는 반드시 _otherwise_에서 잡아서 받은 예외를 그대로 재발생시켜야 합니다. 그렇지않으면 프로그램이 아무도 모르게 실패하게되는 경우가 생깁니다.

`System.IO.Error` 모듈에서는 에러가 발생한 파일의 핸들이나 파일명이 무엇인지와 같은 예외의 상세정보를 물어볼 수 있는 몇가지 함수들을 제공합니다. 이중에서 `ioe`로 시작하는 함수들의 목록은 [full list of them](https://downloads.haskell.org/~ghc/6.10.1/docs/html/libraries/base/System-IO-Error.html#3)에서 확인할 수 있습니다. 예를들어 `ioeGetFileName` 함수를 사용해서 에러를 발생시킨 파일의 이름을 알아낼 수 있습니다. 이 함수의 타입은 `ioeGetFileName :: IOError -> Maybe FilePath`이고, 예외를 통해서 받은 `IOError`를 입력 매개변수로 받아서 `FilePath`\(String의 타입동의어\)를 반환합니다. 이제 예외가 발생했을때, 발생시킨 파일명을 출력하는 프로그램으로 재작성하면 아래와 같습니다.

```haskell
import System.Environment     
import System.IO     
import System.IO.Error     

main = toTry `catch` handler     

toTry :: IO ()     
toTry = do (fileName:_) <- getArgs     
           contents <- readFile fileName     
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"     

handler :: IOError -> IO ()     
handler e     
    | isDoesNotExistError e =   
        case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path  
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"  
    | otherwise = ioError e
```

가드에 의해서 `isDoesNotExistError`가 `True`를 반환할때 `ioeGetFileName` 함수를 호출하기 위해서 _case_ 표현식을 사용하였습니다. 그리고나서 `Maybe`의 값에 대해서 패턴매칭하고, 패턴매칭을 통해서얻은 값으로 파일명을 출력하였습니다. _case_ 구문은 주로 함수 사용없이 어떤것에 대한 패턴매칭을 하고싶을때 사용합니다.

`catch` 구문에서는 여러개의 I/O 영역의 예외를 잡을수도 있습니다. 아래와같이 여러개의 다른 핸들러에 `catch`를 적용할 수 있습니다.

```haskell
main = do toTry `catch` handler1  
          thenTryThis `catch` handler2  
          launchRockets
```

여기서는 `toTry`에서 `handler1`을 사용하고, `thenTryThis`는 `handler2`를 사용합니다. `launchRockets`는 `catch`의 매개변수가 아닙니다. 따라서 어디서 예외가 발생하든 `launchRockets`에서 내부적으로 `catch`를 사용해서 예외처리를 하지않으면 프로그램은 크래쉬가 발생할 것 입니다. 이것은 마치 다른 언어의 _try-catch_ 블록가 유사합니다. 프로그램 전체를 하나의 _try-catch_로 묶을수도 있지만, 예외가 발생하고 그에대한 서로다른 핸들링이 필요한 부분에 대해서 적절하게 나눌수도 있습니다.

여기에서는 순수한 코드에서 예외를 발생시키고 그것을 핸들링하는 것에 대해서는 다루지 않았습니다. 그 이유는 위에서 설명했듯이 하스켈에서는 순수한 코드에서는 예외를 발생시키는 것보다 훨씬 더 좋은 방법이 있기 때문입니다. 심지어 저는 I/O에서 예외가 발생했을때도 `IO (Either a b)`와같이 처리하는 것을 선호합니다.

