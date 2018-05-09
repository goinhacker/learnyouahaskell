# 커맨드라인 매개변수

터미널에서 실행되는 프로그램이나 스크립트를 작성할때는 커맨드라인 매개변수를 다루는 것이 필요합니다. 하스켈에서는 표준 라이브러리에서 프로그램에서 커맨드라인 매개변수를 받아서 처리하는 방법을 제공합니다.

이전 챕터에서 TODO 프로그램에 TODO를 추가하거나 삭제하는 프로그램을 만들었습니다. 두가지 접근 방법을 취했는데, 그중 첫번째는 코드안에 todo.txt 파일명을 하드코딩 하였습니다. 그리고 사용하는 여러개의 TODO 리스트를 관리할 수 없었습니다. 그리고 나머지 한가지 방법은 사용자에게 어떤 TODO 목록을 사용하지를 물어보는 것 입니다. 사용자가 삭제할 항목을 알고싶을때 이런 방법을 사용했습니다. 이번에는 이런 인터렉티브한 동작이 아니고, 한번의 실행으로 동작하는 배치 프로그램 형태로 작성해 보겠습니다. 사용자에게 매번 물어보지 않기위해서 커맨드라인 매개변수를 통해서 필요한 입력을 받습니다.

`System.Environment`는 여러가지 I/O 작업들은 포함하고 있는 모듈입니다. `getArgs`는 `getArgs :: IO [String]` 타입을 가지고 있습니다. 이 함수는 프로그램이 실행될때 매개변수를 받아서 매개변수들의 리스트를 결과로 포함한 I/O 작업을 반환합니다. `getProgName` 함수는 `getProgName :: IO String` 타입이고, 프로그램의 이름을 포함한 I/O 작업을 반환합니다.

```haskell
import System.Environment   
import Data.List  

main = do  
   args <- getArgs  
   progName <- getProgName  
   putStrLn "The arguments are:"  
   mapM putStrLn args  
   putStrLn "The program name is:"  
   putStrLn progName
```

`getArgs`의 결과를 `args`, `getProgName`의 결과를 `progName`에 바인딩 하였습니다. 그리고 어떤 내용을 포함하는지 출력하였습니다. `arg-test`라는 프로그램명으로 실행하면 아래와 같습니다.

```haskell
**[terminal]
**[prompt $ ]**[command ./arg-test first second w00t "multi word arg"]
The arguments are:  
first  
second  
w00t  
multi word arg  
The program name is:  
arg-test
```

이제 이 함수를 사용해서 커맨드라인 매개변수를 사용해서 새로운 TODO를 추가하고 삭제한느 프로그램을 만들어보겠습니다. 또한 `todo.txt` 파일뿐만 아니라 매개변수를 통해서 선택된 파일명에 기록하도록 하겠습니다. 프로그램의 기능은 아래와 같습니다.

* 작업목록 확인
* 작업목록 추가
* 작업목록 삭제

  만약 `Find the magic sword of power`라는 작업을 _todo.txt_ 파일에 추가하고 싶다면, 터밀널에서 `todo add todo.txt "Find the magic sword of power"`와 같이 입력해야 합니다. 작업내역을 확인하려면 `todo view todo.txt`와 같이 입력합니다. 2번 작업을 삭제하고 싶다면, `todo remove todo.txt 2`와 같이 입력합니다. 예제에서는 잘못된 입력에 대해서 고려하지 않겠습니다.

```haskell
import System.Environment   
import System.Directory  
import System.IO  
import Data.List  

dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ]
```

먼저 입력에 따라서 어떤 함수를 수행할지에 대한 연관 리스트를 선언하였습니다. 각 함수의 타입은 `[String] -> IO ()` 입니다. 이 매개변수들을 리스트로 받아서 보기/추가하기/삭제하기등의 I/O 작업을 반환합니다.

```haskell
main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args
```

프로그램의 `main` 함수에서 입력 매개변수를 받아서 `(command:args)`에 바인딩하였습니다. 패턴매칭이 동작해서 `command`에는 매개변수의 첫번째 입력이 들어오고, 나머지 입력들은 `args`에 들어갑니다. 예를들어 터미널에 `todo add todo.txt "Spank the monkey"`와 같이 입력하면, `command`는 `"add"`가되고 `args`는 `["todo.xt", "Spank the monkey"]`가 됩니다.

다음 라인 `lookup command dispatch`에서 `"add"`는 `add`에 맵핑되어 있으므로, `Just add`를 결과로 받습니다. 여기서 맵핑되는 함수를 찾아서 `Maybe`로 패턴매칭하였습니다. 만약 `dispatch` 목록에 없는 입력이 들어온다면 `Nothing`이 됩니다.

마자막으로 나머지 매개변수들로 `action` 함수를 호출하였습니다. `action` 함수는 아이템이 추가됐거나, 아이템 목록을 출력하거나, 아이템을 삭제하는 I/O 작업을 반환할 것 입니다. 이렇게 판단할 수 있는 이유는 `action` 함수는 `main` 함수의 _do_ 블럭의 부분이기 때문입니다. 예를들어 `todo add todo.txt "Spank the monkey"`가 입력되었다면, `action` 함수는 `["todo.txt", "Spank the monkey"]` 입력으로 받아서 `Spank the monkey`를 _todo.txt_ 파일에 추가하면 I/O 작업을 반환합니다.

이제 `add`, `view`, `remove` 함수를 작성해보겠습니다.

```haskell
add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
```

`todo add todo.txt "Spank the monkey"`로 실행되면 `command`가 `"add"`가 되어, 패턴매칭에 의해서 `action`은 `add`가 됩니다. 따라서 마지막 라인에서 `add` 함수를 `["todo.txt", "Spank the monkey"]`를 매개변수로 실행합니다. `add` 함수는 파일의 마지막에 뉴라인 문자를 포함한 새로운 아이템을 추가하는 I/O 작업을 반환합니다.

```haskell
view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks
```

`view` 함수에서는 예를들어 `todo view todo.txt`를 입력받았다면 `command`는 `"view"`가되고 `args`는 `["todo.txt"]`가 됩니다. `view` 함수는 입력 파일의 내용을 읽어서 화면에 출력하는 I/O 작업을 반환합니다.

```haskell
remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName
```

마지막으로 `remove` 함수입니다. `remove` 함수는 이전 챕터에서 작성했던 프로그램의 삭제 기능과 거의 유사하게 동작됩니다. 차이점은 파일명을 `todo.txt`로 하드코딩하지 않고, 입력 매개변수로 부터 받는다는 점입니다. 또한 사용자에게 삭제할 아이템 번호를 묻지않고, 매개변수로 받은 번호를 사용합니다. 역시 마지막에 임시파일을 생성하여 삭제작업을 진행하고, 기존 파일 삭제 후, 임식파일명을 변경하는 방식으로 파일 수정을 진행하였습니다.

프로그램 전체 코드는 아래와 같습니다.

```haskell
import System.Environment   
import System.Directory  
import System.IO  
import Data.List  

dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ]  

main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args  

add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")  

view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks  

remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName
```

프로그램을 요약하면, 먼저 커맨드라인의 매개변수를 받아서 I/O 작업을 반환하는 함수로 맵핑하는 연관 리스트를 만들었습니다. 이미 알고있는 command를 기반으로 연관 리스트에서 적절한 함수를 찾습니다. 찾은 함수를 나머지 매개변수들과 함께 호출하여 적절한 작업을 수행하고, 해당 작업을 수행하는 I/O 작업을 반환합니다.

만약 다른 언어로 작성했다면, switch case문 같은 것을 사용하여 작성했을 것 입니다. 여기서는 고계함수를 사용하여 dispatch 리스트에서 입력 매개변수에 대한 I/O 작업을 제공하는 함수를 찾았습니다. 이제 프로그램의 각 기능을 실행해보면 아래와 같습니다.

```haskell
**[terminal]
**[prompt $ ]**[command ./todo view todo.txt]
0 - Iron the dishes  
1 - Dust the dog  
2 - Take salad out of the oven

**[prompt $ ]**[command ./todo add todo.txt "Pick up children from drycleaners"]

**[prompt $ ]**[command ./todo view todo.txt]
0 - Iron the dishes  
1 - Dust the dog  
2 - Take salad out of the oven  
3 - Pick up children from drycleaners

**[prompt $ ]**[command ./todo remove todo.txt 2]

**[prompt $ ]**[command ./todo view todo.txt]
0 - Iron the dishes  
1 - Dust the dog  
2 - Pick up children from drycleaners
```

이 프로그램의 기능을 확장할때도 간단합니다. 연관 리스트에 맵핑을 추가하고 기능을 수행하는 함수를 작성하면 됩니다. 예를들어, 파일과 작업번호를 입력 받아서 해당 작업을 최상위로 올려주는 `bump` 함수를 추가해볼 수 있습니다.

또한 올바르지 않은 입력에 대한 처리를 추가할 수 있습니다. 예를들어 입력이 올바르지 않으면 에러를 리포팅해주는 I/O 작업을 만들고, \(ex: `errorExit` :: IO \(\)\) 잘못된 입력을 체크하여 에러 리포팅 I/O 작업을 수행하는 할 수 있습니다. 다른 방법으로는 다음에 배울 예외를 사용할 수 있습니다.

