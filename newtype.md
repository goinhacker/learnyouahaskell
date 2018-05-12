# newtype 키워드

## newtype

이번 섹션에서는 _newtype_이라는 하스켈의 새로운 키워드에 대해서 다룹니다. 이전에 대수적\(algebraic\) 데이터 타입을 만들는 _data_ 키워드와  타입동의어를 만드는 _type_ 키워드에 대해서 배웠습니다. _newtype_ 키워드는 기존의 데이터 타입에서 새로운 타입을 만드는 방법입니다. 

#### 그렇다면 _newtype_ 키워드는 어따 쓰나요?

이전 섹션에서 리스트 타입이 어플리케이티브 펑터가 되는 여러가지 방법에 대해서 배웠습니다. 그 중 한가지가 왼쪽 리스트의 함수들과 오른쪽 리스트의 값들의 모든 조합을 적용하는 `<*>` 입니다. 

```haskell
ghci> [(+1),(*100),(*5)] <*> [1,2,3]
[2,3,4,100,200,300,5,10,15]
```

다른 예로 `<*>`의 왼쪽에서 첫번째 함수를 가져와서 오른쪽의 첫번째 값을 적용하고, 그 다음에 두번째 함수와 값에 대해서 적용하는 방식으로 진행한다고 해보겠습니다. 이렇게하면 결과적으로 두개의 리스트가 zipping이 될 것 입니다. 하지만 기존 리스트는 이미 `Applicative`의 인스턴스입니다. 따라서 zipping 방식으로 동작하는 새로운 리스트를 만들어서 `Applicative`의 인스턴스로 만들어야 합니다. 

```haskell
ghci> getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]
[2,200,15]
```

궁극적으로 위와같이 동작하는 `ZipList`라는 새로운 타입을 정의해 보겠습니다. 예제에서 볼수있듯이 `ZipList`는 기존 하스켈의 리스트를 래핑해야 합니다. 따라서 일단 아래와 같이 _data_ 키워드를 사용해서 `ZipList`를 정의할 수 있습니다.

```haskell
data ZipList a = ZipList [a]
```

기존 리스트를 래핑하기 때문에 타입은 하나의 필드를 가진 값 생성자\(value constructor\)가 됩니다. 그리고 최종결과를 리스트로 얻기위해서 `ZipList`안에 래핑된 리스트를 꺼낼 수 있어야 합니다. 이것은 레코드를 사용해서 할 수 있습니다. 

```haskell
data ZipList a = ZipList { getZipList :: [a] }
```

이제 `ZipList`를 `Applicative`의 인스턴스로 만들 수 있을 것입니다. 

지금까지 존재하는 타입으로 부터 새로운 타입을 정의하는 두가지 방법을 알았습니다.

1. 타입클래스의 인스턴스를 만드는 방법
2. _data_ 키워드로 기존 타입을 래핑해서 새로운 타입을 만드는 방법

하스켈의 _newtype_ 키워드는 정확히 동일한 상황에서 사용됩니다. 즉, 존재하는 타입을 새로운 타입으로 래핑하는 것입니다. 따라서 `ZipList a`를 _newtype_ 키워드로 정의하면 아래와 같습니다.

```haskell
newtype ZipList a = ZipList { getZipList :: [a] }
```

#### _newtype_ 키워드는 왜 필요한가요?

_data_ 키워드와 _newtype_ 키워드는 선언 방법이 동일한데 왜 _newtype_ 키워드가 필요할까요? 그 이유는 **이러한 일들을 할때 **_**newtype**_** 키워드가 빠르기 때문입니다. **어떤 타입을 래핑하기 위해서 _data_ 키워드를 쓰면, 프로그램이 실행 중에 타입을 래핑하고 언랭핑하기위한 오버헤드가 발생합니다. _newtype_ 키워드를 쓰면, 하스켈은 이 타입은 기존에 존재하는 타입을 새로운 이름의 타입으로 래핑 했을 뿐이라는 것을 미리 알고 있습니다. 따라서 하스켈은 어떤 타입인지가 결정되면 래핑과 언래핑을 제거할 수 있습니다.

#### 그렇다면 왜 항상 _data_ 키워드 대신 _newtype_ 키워드를 쓰지 않나요?

_**newtype**_** 키워드는 아래와 같은 두가지 제약이 존재**하기 때문입니다.

1. 오직 한개의 값 생성자만 가질 수 있다.
2. 값 생성자는 오직 하나의 필드만 가질 수 있다.

하지만 _data_ 키워드는 아래와 같이 여러개의 값 생성자를 가질 수 있고, 값 생성자는 여러개의 필드를 가질 수 있습니다.

```haskell
data Profession = Fighter | Archer | Accountant  
data Race = Human | Elf | Orc | Goblin  
data PlayerCharacter = PlayerCharacter Race Profession
```

#### _newtype_ 키워드도 _deriving_ 키워드와 함께 사용할 수 있습니다.

newtype 키워드로 정의된 타입은 `Eq`, `Ord`, `Enum`, `Bounded`, `Show`, `Read` 타입클래스의 인스턴스가 될 수 있습니다. 이때 새로 정의된 타입에의해 래핑된 타입이 해당 타입클래스의 인스턴스여야만 합니다. 따라서 아래와 같이 `CharList`를 `Show`와 `Eq` 타입클래스의 인스턴스로 만들면 화면에 출력하고 동등 비교를 할 수 있습니다. 

```haskell
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)
```

실제로 실행해보면 아래와 같습니다. 

```haskell
ghci> CharList "this will be shown!"  
CharList {getCharList = "this will be shown!"}  
ghci> CharList "benny" == CharList "benny"  
True  
ghci> CharList "benny" == CharList "oisters"  
False 
```

예제에서 생성된 `CharList`들의 타입은 모두 아래와 같습니다. 

```haskell
CharList :: [Char] -> CharList
```

따라서 "my sharona"와 같은 `[Char]`을 받아서 `CharList`를 반환합니다. 타입 선언에서 레코드를 사용했기때문에 `getCharList` 함수가 생성되었습니다. 이 함수의 타입은 아래와 같습니다. 

```haskell
getCharList :: CharList -> [Char]
```

함수는 언래핑하기 때문에 값 생성자와 반대로 `CharList`를 받아서 `[Char]`를 반환하는 것을 확인할 수 있습니다.

#### _newtype_으로 타입클래스 인스턴스 만들기























