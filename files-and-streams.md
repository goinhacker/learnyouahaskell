# Files and Streams

## getContents

`getContents` 함수는 표준 입력으로부터 파일의 끝을 만날때까지 전부 읽어들이는 I/O 작업입니다. 이 함수의 타입은 `getContents :: IO String` 입니다. 이 `getContents` 함수의 가장 큰 장점은 게으른\(lazy\) I/O라는 점 입니다. 따라서 호출된 시점에 읽어들이지 않고, 값이 정말 필요한 시점에 콘솔에서 입력을 읽을 것입니다.

`getContents`는 어떤 프로그램의 출력은 다른 프로그램에 입력을 넣는 파이프라이닝을 할때 매우 유용한 함수입니다. 여기서는 유닉스 시스템에서 파이프라인 작업이 어떻게 동작하는지 살펴보겠습니다. 먼저 아래와 같은 텍스트 파일을 만들었습니다.

```text
I'm a lil' teapot  
What's with that airplane food, huh?  
It's so small, tasteless
```

이제는 이전에 소개드렸던 `forever` 함수를 사용해서 아래와 같은 프로그램을 만들었습니다.

```haskell
import Control.Monad  
import Data.Char  

main = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l
```

이 프로그램은 사용자에게 한 라인을 받아서 대문자로 바꾸는 작업을 계속 반복합니다. 이 프로그램을 컴파일하고 위에서 만든 텍스트 파일을 넣어보면 아래와 같이 동작합니다.

```haskell
$ ghc --make capslocker
[1 of 1] Compiling Main             ( capslocker.hs, capslocker.o )
Linking capslocker ...
$ cat haiku.txt
I'm a lil' teapot  
What's with that airplane food, huh?  
It's so small, tasteless
$ cat haiku.txt | ./capslocker
I'M A LIL' TEAPOT  
WHAT'S WITH THAT AIRPLANE FOOD, HUH?  
IT'S SO SMALL, TASTELESS
capslocker <stdin>: hGetLine: end of file
```

여기서는 유닉스의 `cat`과 `|`를 사용해서 위에서만든 capslocker 프로그램에 전달하였습니다. 이것은 콘솔에서 capslocker를 실행하고 콘솔에서 텍스트 파일을 입력하고, EOF를 발생시키기 위해서 Ctrl-D를 누르는 작업과 동일합니다. 여기서 `cat haiku.txt | ./capslocker`는 바로 터미널에 출력되지않고, capslocker 프로그램에 전달되었습니다.

`forever`를 사용할때는 필수적으로 입력을 받아서 출력으로 변환해야 합니다. 이러한 작업을 간단하게 처리하기 위해서 `getContents`를 사용합니다.

```haskell
import Data.Char  

main = do  
    contents <- getContents  
    putStr (map toUpper contents)
```

이 예제는 `getContents` 수행해서 받은 결과를 `contents`에 바인딩 하였습니다. 그리고 나서 `toUpper` 함수를 사용해서 대문자로 변경한 후, 콘솔에 출력합니다. 이 예제에서 문자열은 본래 리스트이기 때문에 lazy하게 동작하고, `getContents`는 위에서도 언급한 것처럼 lazy I/O입니다. 또한 한번에 전체를 읽지않기 때문에 화면에 출력하기 전에 메모리에 저장하고, 정말 필요할때 입력으로부터 한 라인씩만 읽어서 화면에 출력합니다.

```haskell
$ cat haiku.txt | ./capslocker
I'M A LIL' TEAPOT  
WHAT'S WITH THAT AIRPLANE FOOD, HUH?  
IT'S SO SMALL, TASTELESS
```

`capslocker` 프로그램을 직접 실행하면 아래와 같이 라인단위로 입력하고 출력할 수 있습니다.

```haskell
$ ./capslocker
hey ho  
HEY HO  
lets go  
LETS GO
```

`capslocker`프로그램은 `getContents`에서 `contents`에 바인딩하고 `toUpper`로 변환할때까지 값에 대한 평가가 이루어 지지 않습니다. `putStr`를 수행했을때 비로소 이전에 수행하기로 했던 동작들을 수행하고 문자열을 만들어 출력합니다. 그리고 EOF를 만날때까지 계속해서 `putStr`은 종료되지 않고 이 동작들을 반복하게 됩니다.

## interact

이번에는 어떤 입력을 받아서 한 라인이 10개 이하의 문자일때만 출력하는 프로그램을 만들어 보겠습니다.

```haskell
main = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)  

shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result
```

이 프로그램에서는 `shortLinesOnly`로 문자열을 처리하는 부분을 묶어서 I/O가 발생하는 부분을 최소화하였습니다. `shortLinesOnly` 함수는 문자열을 입력으로 받아서 문자열을 리턴하는 함수입니다. `shortLinesOnly`은 아래와 같이 동작합니다.

1. `"short\nlooooooooooooooong\nshort again"`와 같은 문자열을 입력 받음
2. 입력 문자열에 `lines` 함수를 적용 -&gt; `["short", "looooooooooooooong", "short again"]`
3. 리스트의 각 문자열을 길이가 10이하인 것으로 필터링 -&gt; `["short", "short again"]`
4. `unlines`를 수행하여 하나의 새로운 라인으로 병합 -&gt; `"short\nshort again"`

아래 다른 입력으로 실행해본 예제입니다.

```text
i'm short  
so am i  
i am a loooooooooong line!!!  
yeah i'm long so what hahahaha!!!!!!  
short line  
loooooooooooooooooooooooooooong  
short
```

```haskell
$ ghc -make shortlinesonly
[1 of 1] Compiling Main             ( shortlinesonly.hs, shortlinesonly.o )  
Linking shortlinesonly ... 
$ cat shortlines.txt | ./shortlinesonly
i'm short  
so am i  
short
```

위 프로그램과 같이 입력으로부터 어떤 문자열을 읽어서 함수로 변환한 다음 출력하는 패턴은 `interact` 함수를 사용하여 더 간단히 할 수 있습니다. `interact` 함수는 `String -> String` 형태의 함수를 매개변수로 받고, 어떤 입력을 받아서 함수의 결과를 출력하는 I/O 작업을 반환합니다. 위에서 작성한 함수를 `interact`를 사용해서 재작성하면 아래와 같습니다.

```haskell
main = interact shortLinesOnly  

shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result
```

이 예제에서는 코드는 약간 더 간결하고 읽기쉬워졌습니다. 함수 합성을 하면 아래와 같이 더 간결하게 작성할 수 있습니다.

```haskell
main = interact $ unlines . filter ((<10) . length) . lines
```

이번 예제에서는 코드가 한라인으로 간결해진 것을 확인할 수 있습니다.

`interact`는 예를들어 아래와 같은 프로그램을 만들때 사용됩니다.

* 어떤 컨텐츠를 파이프에 담고 결과를 덤프하는 프로그램
* 사용자 입력의 한라인을 받아서, 그 라인을 기반으로 어떤 결과를 돌려주고 다른 라인을 받는 프로그램

이번에는 계속해서 한라인을 읽고, 라인이 palindrome\(앞으로 읽으나 뒤로 읽으나 같은 문장\)인지 알려주는 프로그램을 만들어보겠습니다. 간단하게 `getLine` 함수로 한라인을 읽어와서 palindrome인지 확인하여 알려주고, 다시 `main` 을 수행하면 됩니다. `interact`를 사용하면 더 간단해 집니다. 입력을 어떻게하면 원하는 출력으로 바꿀 수 있을지만 고민하면 됩니다. 따라서 각 입력 라인을 확인하여 `"palindrome"` 또는 `" not a palindrome"` 문자열로 변환하는 함수를 만들어 보겠습니다.

```haskell
respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))  
    where   isPalindrome xs = xs == reverse xs
```

이것을 point-free로 다시 작성하면 아래와 같습니다.

```haskell
respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines  
    where   isPalindrome xs = xs == reverse xs
```

이 함수에 `"elephant\nABCBA\nwhatever"`를 넣어보면 아래와 같이 동작할 것입니다.

1. 입력 문자열을 라인 단위의 리스트로 변화 -&gt; `["elephant", "ABCBA", "whatever"]`
2. 각 라인 문자열을 람다함수에 넣어서 변환 -&gt; `["not a palindrome", "palindrome", "not a palindrome"]`
3. `unlines`를 수행하여 한라인으로 병합 -&gt; `"not a palindrome\npalindrome\nnot a palindrome"`

이제 이렇게 만들어진 함수를 `interact` 함수에 적용하기만 하면 됩니다.

```haskell
main = interact respondPalindromes
```

```haskell
$ runhaskell palindromes.hs
hehe  
not a palindrome  
ABCBA  
palindrome  
cookie  
not a palindrome
```

interact 함수를 사용해서 하나의 큰 문자열을 다른 입력 문자열로 변환하는 프로그램을 라인 단위의 프로그램으로 만들었습니다. 하스켈은 게으르게 실행되기 때문에 입력 문자열의 첫번째 라인이 들어왔을때 바로 출력하지 않습니다. 실제로 출력이 필요한 시점에 첫번째 라인을 출력하고, EOF를 만나면 프로그램이 종료됩니다.

이 프로그램을 어떤 파일의 내용을 파이핑하여 동작하도록 할 수 있습니다.

```text
dogaroo  
radar  
rotor  
madam
```

위와같은 내용을 담은 파일 `words.txt`가 있다고 했을때,

```haskell
$ cat words.txt | runhaskell palindromes.hs
not a palindrome
palindrome
palindrome
palindrome
```

직접 타이핑하지 않고, 파이프를 통해서 프로그램에 입력되는 것을 확인할 수 있다. 이처럼 Lazy I/O에서는 꼭 필요하기 전까지는 입력을 소비하지 않습니다.

지금까지는 콘솔에 출력하거나 콘솔에서 읽어들이는 I/O 작업에 대해서 알아보았다. 여기서는 파일에 쓰고 읽는 방법을 알아보자. 콘솔에 읽고 쓰는 작업은 `stdout`, `stdin`이라는 두개의 파일에 읽고 쓰는 작업과 같다. 즉, 파일에 읽고 쓰는 작업은 콘솔에 읽고 쓰는 작업과 매우 유사하다.

## openFile, hGetContents, hClose

`girlfriend.txt` 파일을 열어서 콘솔에 그대로 출력하는 간단한 프로그램을 작성해보자.

```text
Hey! Hey! You! You!   
I don't like your girlfriend!   
No way! No way!   
I think you need a new one!
```

먼저 `girlfriend.txt` 파일을 만들고 내용을 입력하였습니다.

```haskell
import System.IO  

main = do  
    handle <- openFile "girlfriend.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle
```

프로그램을 만들고, 실행해보면 아래와 같이 파일의 내용이 콘솔에 그대로 출력되는 것을 확인할 수 있습니다.

```haskell
$ runhaskell girlfriend.hs
Hey! Hey! You! You!  
I don't like your girlfriend!  
No way! No way!  
I think you need a new one!
```

프로그램을 살펴보면 첫번째 라인에서 `openFile` 함수를 사용하였다. 이 함수의 타입 선언은 `openFile :: FilePath -> IOMode -> IO Handle` 이다. `openFile` 함수는 파일의 경로와 `IOMode`를 받아서 파일을 열어서 파일과 연결된 핸들을 받아오는 I/O 작업을 반환하는 함수입니다.

`FilePath`는 아래와 같이 선언된 `String`의 [타입동의어](https://jaeyongcho.gitbooks.io/learnyouahaskell/d0c0-c785-b3d9-c758-c5b4.html)입니다.

```haskell
type FilePath = String
```

`IOMode`는 아래와 같이 정의된 [타입](https://jaeyongcho.gitbooks.io/learnyouahaskell/making-types.html)입니다.

```haskell
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
```

`IOMode`는 단순한 열거형으로 파일의 모드를 의미합니다. `IO Mode`는 `Mode`라는 타입의 I/O 작업을 의미하니 혼돈하지 않아야 합니다.

마지막으로 `openFile` 함수는 지정된 모드로 열리는 파일의 I/O 작업을 반환합니다. 이 I/O 작업을 바인딩하면 `Handle`을 얻을 수 있습니다. `Handle` 타입의 값은 파일의 위치를 나타냅니다. `Handle`을 사용해서 어디에서 어떤 파일을 읽어야 하는지 알 수 있습니다. 위 예제 프로그램에서는 `openFile` 함수의 반환값 `Handle`을 `handle`에 바인딩 하였습니다.

예제의 다음 라인에서 `hGetContents` 함수를 호출하였습니다. 이 함수는 `Handle`을 받아서 파일의 내용을 가져와서 `IO String`을 반환합니다. `hGetContents` 함수는 `getContents` 함수와 상당히 유사한데, 유일한 차이점은 `getContents` 함수는 표준 입력에서 자동으로 컨텐츠를 읽어오는 반면에, `hGetContents` 함수는 파일의 핸들로부터 읽어온다는 점 입니다. 따라서 두 함수 모두 메모리를 활용하여 꼭 필요한 시점에 데이터를 읽어옵니다. `hGetContents` 함수를 통해 파일의 전체 내용을 `contents`로 바인딩해서 사용하는데 실제로 메모리에 로딩되지는 않습니다. 그래서 매우 큰 파일이 들어와도 필요할때 필요한 것만 읽어서 메모리에 무리를 주지 않을 수 있습니다.

파일을 식별하기 위해서 사용하는 핸들과 파일의 컨텐츠 사이의 차이점에 유의해야 합니다. 핸들은 단지 어떤 파일인지 알기위한 것 입니다. 파일 시스템을 하나의 거대한 책으로 간주한다면, 각 챕터가 파일이고, 핸들은 현재 당신이 읽거나 쓰고있는 챕터를 보여주는 북마크 입니다. 반면에 컨텐츠는 실제 해당 챕터의 내용입니다.

다음으로 `putStr contents`로 표준 출력에 컨텐츠를 출력하고, 핸들을 받아서 닫을 파일의 I/O 작업을 반환하는 `hClose`를 사용하였습니다. `openFile`을 사용해서 열었던 파일은 사용 후, 반드시 닫아야 합니다.

## withFile

다른 방법으로 `withFile` 함수를 사용할 수 있습니다. 이 함수의 타입은 `withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a` 입니다. 파일의 경로와 `IOMode`, 핸들을 받아서 어떤 I/O 작업을 반환하는 함수를 입력을 받습니다. 그리고 파일을 열고 파일에서 원하는 작업을 수행한 다음에 닫는 I/O 작업을 반환합니다. 최종적으로 I/O 작업으로 캡슐화된 결과는 입력으로 주어진 함수가 반환하는 I/O 작업의 결과와 동일합니다. 조금 복잡하게 느껴지지만, 아래 이전에 내왔던 예제를 `withFile` 함수를 사용하는 것으로 재작성해보겠습니다.

```haskell
import System.IO     

main = do     
    withFile "girlfriend.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle     
        putStr contents)
```

람다로 정의된 `\handle -> ...` 부분은 이전에 작성한 코드의 일부분과 유사합니다. 여기서는 입력받은 핸들로 파일을 열어서 주어진 동작을 수행하고, 파일은 닫습니다. 이전에 작성한 프로그램에서는 직접 파일을 열고 닫았지만,`withFile` 함수는 자동으로 이 작업을 수행합니다. `withFile` 함수를 직접 작성하면 아래와 같습니다.

```haskell
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
withFile' path mode f = do  
    handle <- openFile path mode   
    result <- f handle  
    hClose handle  
    return result
```

입력받은 파일의 경로, IOMode로 `openFile`을 호출하여 핸들을 만들고, 만들어진 핸들을 입력받은 람다 함수의 입력으로 넘깁니다. 람다함수의 수행 결과를 `result`에 바인딩하고 `hClose`를 호출하여 핸들을 닫은 후에 `result`를 반환하였습니다. 따라서 위에서 입력받은 함수 `f`의 결과가 `withFile` 함수의 결과은 동일한 I/O 작업으로 캡슐화된 결과입니다.

`hGetContents`가 `getContents`와 동작은 동일하지만 파일과의 I/O로 특정되었던 것처럼, `hGetLine`, `hPutStr`, `hPutStrLn`, `hGetChar` 함수 등도 동일합니다. 표준입출력 대신 핸들을 입력받아서 파일 I/O를 다루는 함수명에는 _h_가 붙어있습니다.

## readFile

`readFile` 함수의 타입은 `readFile :: FilePath -> IO String` 입니다. 여기서 `FilePath`는 `String`의 다른 이름일 뿐입니다. `readFile`은 파일의 경로를 받아서 파일을 \(게으르게\) 읽고 그 내용을 문자열로 바인딩 합니다. 대게는 `openFile`을 사용하여 핸들에 바인딩하고 `hGetContents` 함수를 사용하는 것보다 편리합니다. 이전에 작성한 예제를 `readFile`을 사용하여 다시 작성하면 아래와 같습니다.

```haskell
import System.IO  

main = do  
    contents <- readFile "girlfriend.txt"  
    putStr contents
```

파일을 식별하기 위한 핸들을 사용하지 않기 때문에 수동으로 파일을 Close할 수 없습니다. 따라서 `readFile`을 사용할때는 하스켈이 자동으로 파일을 닫아줍니다.

## writeFile

`writeFile` 함수의 타입은 `writeFile :: FilePath -> String -> IO ()` 입니다. 파일의 경로와 파일에 쓸 문자열을 입력받아서 쓰는 I/O 작업을 반환합니다. 만약 파일이 이미 존재한다면 쓰기전에 길이가 0으로 내려갑니다. CAPSLOCKED에 girlfriend.txt를 적용해서 girlfriendcaps.txt 파일을 작성하는 프로그램을 만들어서 실행해보면 아래와 같습니다.

```haskell
import System.IO     
import Data.Char  

main = do     
    contents <- readFile "girlfriend.txt"     
    writeFile "girlfriendcaps.txt" (map toUpper contents)
```

```haskell
$ runhaskell girlfriendtocaps.hs
$ cat girlfriendcaps.txt
HEY! HEY! YOU! YOU!  
I DON'T LIKE YOUR GIRLFRIEND!  
NO WAY! NO WAY!  
I THINK YOU NEED A NEW ONE!
```

## appendFile

`appendFile` 함수의 타입은 `appendFile :: FilePath -> String -> IO ()`으로 `writeFile`의 타입과 같다. 단지 `appendFile`은 파일이 이미 존재하지만 그 파일에 컨텐츠를 추가하는 경우, 길이를 0으로 자르지 않습니다. 해당할 일들의 목록을 라인당 하나씩 가지고 있는 todo.txt 파일에 표준 입력으로 todo 리스트를 추가하는 프로그램을 작성해서 실행해 보면 아래와 같습니다.

```haskell
import System.IO     

main = do     
    todoItem <- getLine  
    appendFile "todo.txt" (todoItem ++ "\n")
```

```haskell
$ runhaskell appendtodo.hs
Iron the dishes
$ runhaskell appendtodo.hs
Dust the dog
$ runhaskell appendtodo.hs
Take salad out of the oven  
$ cat todo.txt
Iron the dishes  
Dust the dog  
Take salad out of the oven
```

`getLine` 함수는 끝에 뉴라인 문자가 없기때문에 `"\n"`을 추가하였습니다.

이전에 `contents <- hGetContents handle`은 게으르게 동작하기 때문에 파일의 내용 전체를 한번에 메모리에 올리지 않는다고 했었습니다.

```haskell
main = do   
    withFile "something.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle  
        putStr contents)
```

이 프로그램은 사실 파일의 출력을 파이프로 연결한 것과 같습니다. 리스트를 스트림으로 생각할 수 있는 것 처럼 파일을 스트림으로 생각할 수 있습니다. 이 프로그램은 한번에 한라인만 읽어서 콘솔에 출력합니다. 따라서 한번에 파일로부터 읽어들이는 가장 작은 단위는 한 라인입니다. 파일을 읽을때는 한라인에 대한 버퍼만 할당해서 메모리에 로딩합니다. 바이너리 파일을 읽을때는 보통 block-buffering을 기본으로 하는데, 이것은 파일의 청크 단위로 읽는다는 의미입니다. 청크의 크기는 운영체제에서 관리합니다.

이런 버퍼링은 `hSetBuffering` 함수를 사용하여 컨트롤할 수 있습니다. 이 함수는 `BufferMode`를 받아서 버퍼를 셋팅하는 I/O 작업을 반환합니다. `BufferMode`는 `NoBuffering`, `LineBuffering`, `BlockBuffering (Maybe Int)`를 포함하는 열거형 데이터 타입입니다. `Maybe Int`는 청크의 크기를 의미합니다. 만약 `Nothing`으로 설정하면 청크의 크기는 운영체제에서 결정합니다. `NoBuffering`은 한번에 한개의 문자를 읽는 설정입니다. `NoBuffering`은 디스크 접근이 매우 많기때문에 거의 사용되지 않습니다.

아래와 같이 작성하면 라인 단위로 읽지 않고, 2048 바이트 단위로 파일을 읽어드립니다.

```haskell
main = do   
    withFile "something.txt" ReadMode (\handle -> do  
        hSetBuffering handle $ BlockBuffering (Just 2048)  
        contents <- hGetContents handle  
        putStr contents)
```

청크의 크기를 더 크게할수록 파일을 읽을때 디스크 접근을 줄일 수 있고, 파일이 느린 네크워크 리소스일때 유용하게 사용됩니다.

## hFlush

`hFlush`는 핸들에 연관된 파일의 버퍼를 초기화해주는 함수로 핸들을 받아서 I/O 작업을 반환합니다. 라인 버퍼링을 할때는 모든 라인을 읽은 후에 버퍼가 flush 됩니다. 블럭 버퍼링을 할때는 청크를 읽은 뒤에 flush됩니다. 이런 flush도 직접 컨트롤할 수 있습니다. `hFlush`를 사용하여 지금까지 읽은 데이터를 강제로 flush할 수 있습니다. 데이터가 flushing된 후에는 런타임에 다른 프로그램에서 해당 데이터에 접근할 수 있습니다.

## openTempFile

위에서 작성한 TODO 리스트를 추가하는 프로그램에 아이템을 삭제하는 기능을 추가해 보겠습니다. 이 예제에서는 `System.Directory`와 `System.IO`에 존재하는 새로운 함수들을 사용할 것입니다.

```haskell
import System.IO  
import System.Directory  
import Data.List  

main = do        
    handle <- openFile "todo.txt" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents     
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks     
    putStrLn "These are your TO-DO items:"  
    putStr $ unlines numberedTasks  
    putStrLn "Which one do you want to delete?"     
    numberString <- getLine     
    let number = read numberString     
        newTodoItems = delete (todoTasks !! number) todoTasks     
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile "todo.txt"  
    renameFile tempName "todo.txt"
```

가장 먼저 todo.txt 파일을 열어서 `handle`에 바인딩 하였습니다.

그 다음 `System.IO`에 있는 `openTempFile` 함수를 사용하였습니다. 함수명에서 알 수 있듯이 임시 디렉토리 경로와 파일의 템플릿 명을 받아서 임시 파일을 열어주는 함수 입니다. 예제에서는 `.`로 현재 디렉토리를 입력으로 주었고, `temp`라는 임시 파일의 파일명을 입력으로 주었습니다. 실제로 임시파일이 생성될 때는 `temp`라는 이름뒤에 랜덤 문자들이 붙어서 만들어 집니다. 이 함수는 임시파일을 만들고, I/O 작업안에 임시파일명과 그 파일의 핸들을 페어로 담아서 반환합니다. `openTempFile` 함수를 사용하면 todo2.txt와 같은 파일을 만들어서 덮어쓰지 않고도 임시파일을 생성할 수 있습니다.

현재 디렉토리 경로를 얻기위해서 `.`을 대신해서 `getCurrentDirectory` 함수를 사용할 수도 있습니다.

todo.txt 파일의 내용을 `contents`에 바인딩 하였습니다. 그리고나서 라인단위의 문자열 리스트로 분리하였습니다. 그래서 `todoTasks`는 `["Iron the dishes", "Dust the dog", "Take salad out of the oven"]`와 같이 됩니다. 여기에 zip 함수를 사용하여 각 라인에 번호를 붙여주었습니다. 따라서 `numberedTasks`는 `["0 - Iron the dishes", "1 - Dust the dog" ...`와 같이 변환해줍니다. \(이 작업은 `mapM putStrLn numberedTasks`와 같이 작성할 수도 있습니다.\)

`unlines` 함수를 사용해서 `numberedTasks`의 문자열들을 뉴라인\(`\n`\)을 구분자로한 하나의 문자열로 합쳤습니다. 그리고 이렇게 합쳐진 문자열을 콘솔에 출력합니다.

삭제하기 원하는 TODO 리스트의 번호를 입력받습니다. 만약 1을 입력받았다면 `numberString`은 `"`"`이 됩니다. 이것을`read`를 사용하여 숫자`1`로`number`에 바인딩 합니다. 그리고`delete \(todoTasks !! number\) todoTasks`로 리스트에서 해당 TODO 아이템을 삭제하였습니다. 여기서`!!`는 리스트의 인덱스를 의미합니다.`delete`함수는 리스트에서 해당 아이템을 삭제하고, 새로운 리스트를 반환합니다.`\(todoTasks !! number\)`는`Dust the dog`를 반환합니다. 그리고`Dust the dog`가 없는`todoTasks`를`newTodoItems\`에 바인딩합니다.

임시파일에 쓰기전에 `unlines` 함수로 `newTodoItems`를 하나의 문자열로 합칩니다. 이렇게해서 기존 파일은 수정하지 않고, 하나의 아이템만 삭제된 새로운 임시파일을 만들었습니다.

마지막으로 사용된 핸들들을 `hClose` 함수로 닫고, `removeFile` 함수를 사용하여 기존의 파일을 삭제하고, `renameFile` 함수를 사용하여 임시파일의 파일명을 todo.txt로 변경하였습니다. `removeFile`과 `renameFile` 함수는 입력을 파일경로를 받습니다.

더 적은 라인으로 프로그램을 작성할 수도 있지만, 기존 파일을 덮어쓰지 않도록 임시 파일을 생성하여 프로그램을 만들었습니다. 이제 실행해보겠습니다!!

```haskell
$ runhaskell deletetodo.hs
These are your TO-DO items:  
0 - Iron the dishes  
1 - Dust the dog  
2 - Take salad out of the oven  
Which one do you want to delete?  
1

$ cat todo.txt
Iron the dishes  
Take salad out of the oven

$ runhaskell deletetodo.hs
These are your TO-DO items:  
0 - Iron the dishes  
1 - Take salad out of the oven  
Which one do you want to delete?  
0  

$ cat todo.txt
Take salad out of the oven
```

