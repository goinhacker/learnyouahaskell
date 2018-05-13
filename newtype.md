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

이전 섹션에서 특정 타입클래스의 인스턴스인 사용자 정의 타입을 만드는 것을 배웠습니다. 하지만 타입의 타입 매개변수에 따라서 타입을 정의하기가 어려울 때도 있습니다. 정의하기 쉬운 예로는 `Functor`의 인스턴스로 `Maybe`를 만드는 것입니다. 

```haskell
class Functor f where  
    fmap :: (a -> b) -> f a -> f b  
```

`Functor` 타입클래스 정의는 타입 매개변수 `f` 자리에 그대로 `Maybe`가 들어가면 됩니다. 따라서 아래와 같이 쉽게 `Maybe` 타입을 정의할 수 있었습니다. 

```haskell
instance Functor Maybe where
```

이제 아래와 같이 `Maybe`의 `fmap` 함수만 작성하면 `Maybe` 타입의 정의가 완료됩니다.

```haskell
fmap :: (a -> b) -> Maybe a -> Maybe b
```

여기서  `Maybe`는 단 한개의 타입 매개변수만 필요하기 때문에 쉽게 `Functor`의 인스턴스로 정의될 수 있었습니다.

만약 튜플을 `Functor`의 인스턴스로 만들고, `fmap` 함수에 적용했을때 튜플의 첫번째 값이 변환되도록 만든다면 어떻게 할 수 있을까요?  이 경우, `fmap (+3) (1, 1)`의 결과는 `(4, 1)`이 될 것입니다. 하지만 `fmap` 함수는 `(a, b)`의 첫번째 타입 매개변수 `a`만 바꾸고 끝나기 때문에 `Maybe`와 같은 방식으로는 처리가 불가능합니다.

이 문제를 해결하려면, 두번째 타입 매개변수는 튜플의 첫번째 값의 타입을 나타내는 튜플의 `newtype`을 만들어야 합니다.

```haskell
newtype Pair b a = Pair { getPair :: (a,b) } 
```

이제 튜플의 첫번째 값만 바꾸는 `Functor`의 인스턴스를 만들어보면 아래와 같습니다.

```haskell
instance Functor (Pair c) where  
    fmap f (Pair (x, y)) = Pair (f x, y)
```

`instance Functor (Pair c) where`에서 `Pair c`는 `Functor`의 정의에서 `f`에 들어갑니다. 여기서  `c`는 `Pair b a` newtype 정의에 따라서 튜플의 두번째 매개변수 `b`이고, 이 부분은 `f` 함수에 의해서 변환되면 안되기 때문에 고정값입니다.

그리고 `fmap` 함수의 두번째 매개변수로 받은 튜플을 패턴매칭에 의해서 `Pair (x,y)`로 매칭됩니다. \(여기서 _newtype_으로 정의된 타입은 패턴매칭이 가능하다는 것을 알수 있습니다.\) 이렇게 얻은 입력 튜플 `(x, y)`로 첫번째 값 `x`만 `f` 함수에 적용한 `(f x , y)`를 만들었습니다. 그리고나서 `Pair (f x, y)`로 값 생성자를 호출하여 `Pair b a`로 바꾸어 주었습니다. 완성된 `fmap` 함수의 타입은 아래와 같습니다. 

```haskell
fmap :: (a -> b) -> Pair c a -> Pair c b
```

이제 튜플을 `Pair b a`로 바꾸면, `fmap`을 사용할 수 있고, 함수 `(a -> b)`는 첫번째 값에 매핑됩니다.

```haskell
ghci> getPair $ fmap (*100) (Pair (2,3))  
(200,3)  
ghci> getPair $ fmap reverse (Pair ("london calling", 3))  
("gnillac nodnol",3)
```





