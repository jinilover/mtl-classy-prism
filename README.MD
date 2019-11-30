# Why MTL?  Why classy prism?

MTL and classy prism are not related to each other.  In this example, just happened they are used to address different problems originating from the same source.  And this coincidence is not unusual.

## Background 

This application parses data from an html file and sends it to a REST API.

## MTL - abstraction on monad stack

Before discussing mtl, let's reconsider the 3 functions.

__parseIpipFile__

It is located at https://github.com/jinilover/mtl-classy-prism/blob/master/src/lib/Ipip/Parsers.hs.  Ignore its current type signature, think about what `parseIpipFile` does and the involved monads
* **Reading** the html file (`IO`)
* Parsing the file content that may encounter **invalid data** (`Either e`).

Therefore the return type is expected to be wrapped by a monad stack.

```
parseIpipFile :: UserInfo -> ExceptT e IO BigFiveResult
```

__submitBigFive__

 It is located at https://github.com/jinilover/mtl-classy-prism/blob/master/src/lib/Bot/Client.hs. Again, ignore its current type signature.  What `submitBigFive` does is 
 * Using an argument `BotEnv` to talk to the Recruitbot API (`Reader`).  
 * After submitting the POST requirement, the servant API returns `IO (Either e Text)`.  
 
 Therefore the return type is wrapped by another monad stack

```
submitBigFive :: BigFiveResult ->  ReaderT BotEnv (ExceptT e IO) Text
```

__validateOpts__

It is located at https://github.com/jinilover/mtl-classy-prism/blob/master/src/lib/Bootstrap.hs, it validates `UserOpts` which may contain invalid email text.  Therefore the return type will be

```
validateOpts :: UserOpts -> Either e UserInfo
```

### Problems

These 3 functions are be executed sequentially in `bootstrap` at https://github.com/jinilover/mtl-classy-prism/blob/master/src/lib/Bootstrap.hs.  However, they do not return the same monad.  A possible monad stack that satisfies all of them is `ReaderT BotEnv (ExceptT e IO) a`.  

But this introduce other problems:

* Unnecssary coupling.  E.g. `validateOpts` doesn't need `ReaderT BotEnv`.  It doesn't even need `IO` in `ExceptT e IO a`.  Using `ReaderT BotEnv (ExceptT e IO) a` makes it coupled with its client.  On top of that, the implementation must be added unnecessary logic for `ReaderT BotEnv` and `IO`.
* Inflexibility.  Similar to the last problem.  `parseIpipFile` doesn't need `ReaderT BotEnv`.  Besides, when it is tested using the property test framework, its client is using another monad, `PropertyT IO`.

### Solution - MTL

We don't want a rigid monad stack making the function types tightly coupled with a particular client.  Abstraction bounded by type class constraints solves the problem.  MTL comes to the rescue.  

3 MTL type classes are used for these functions - `MonadReader`, `MonadError` and `MonadIO`.  Now, improve these functions by using these type classes.

```
parseIpipFile 
  :: (MonadError e m, AsParseError e, MonadIO m) 
  => UserInfo -> m BigFiveResult
```
Ignore `AsParseError` which will be discussed in the section "Classy Prisms".  The type classes gives the function a flexibility of not fixing to a particular monad but still enables the function to performs IO and return failure parse result.

Similarly,
```
submitBigFive 
  :: (MonadReader BotEnv m, MonadError e m, AsHttpError e, MonadIO m)
  => BigFiveResult -> m Text
```
```
validateOpts 
  :: (MonadError e m, AsOptError e) 
  => UserOpts -> m UserInfo
```
Ignore `AsHttpError` and `AsOptError` at the moment.

When these functions are executed sequentially
```
validateOpts opts >>= parseIpipFile >>= submitBigFive
```
The resulting monad `m` will be bounded by the following constraints.
```
MonadError e m, MonadReader BotEnv m, MonadIO m, AsOptError e, AsParseError e, AsHttpError e
```
To find out a monad that satisfies the 3 class constraints in `bootstrap` function, one possible solution to check what type class instances are available.  The following instances are selected to give us some hints:
```
instance [safe] MonadError e m 	=> MonadError e (ReaderT r m)   -- 1
instance [safe] Monad m         => MonadError e (ExceptT e m)   -- 2
instance [safe] Monad m         => MonadReader r (ReaderT r m)  -- 3
instance [safe] MonadReader r m => MonadReader r (ExceptT e m)  -- 4
instance [safe] MonadIO m       => MonadIO (ExceptT e m)        -- 5
instance [safe] MonadIO m       => MonadIO (ReaderT r m)        -- 6
instance [safe] MonadIO IO                                      -- 7
```
On line 1, `ReaderT r m` satsify `MonadError` if `m` here satisfy `MonadError` as well.  Line 2 shows that `ExceptT e m` can be the `m`.  Therefore `ReaderT r (ExceptT e m)` satisfies `MonadError`.  Let's check the remaining constraints.

Line 3 shows that `ReaderT r (ExceptT e m)` satisfies `MonadReader`.

Line 6 shows that `ReaderT r (ExceptT e m)` satisfies `MonadIO` if `ExceptT e m` satisfies `MonadIO`.  Line 5 shows that `ExceptT e m` satsifies `MonadIO` if `m` satisfies `MonadIO`.  Line 7 shows `m` can be `IO`.

Therefore the answer is `ReaderT r (ExceptT e IO)` for the `bootstrap` function which is perfect given the type is `IO ()`

By using a similar skill, we find out `ExceptT e (ReaderT r IO)` also satisfies the requirement.

### Other advantages

It is supported in other open-sourced libraries.  E.g. in `lens`
```
view :: MonadReader s m => Getting a s a -> m a
```

Another example is hedgehog.  The property-based test return type is `PropertyT IO ()`.

hedgehog provides the following type class instances
```
instance MonadReader r m  => MonadReader r (PropertyT m)
instance MonadError e m   => MonadError e (PropertyT m)
instance MonadIO m        => MonadIO (PropertyT m)
```
Therefore any function bounded by `MonadError`, `MonadReader` or `MonadIO` can be integrated easily with hedgehog.

Reference:

* https://blog.jle.im/entry/mtl-is-not-a-monad-transformer-library.html
* https://ocharles.org.uk/posts/2016-01-26-transformers-free-monads-mtl-laws.html
* https://serokell.io/blog/tagless-final - the first example illustrates how to apply MTL.

## Classy Prisms

Going back to the 3 functions.  Imagine what will happen if classy prisms are not used on the error types.  It will be something like
```
parseIpipFile 
  :: (MonadError ParseError m, MonadIO m)
  => UserInfo -> m BigFiveResult

submitBigFive 
  :: (MonadReader BotEnv m, MonadError HttpError m, MonadIO m)
  => BigFiveResult -> m Text

validateOpts :: (MonadError OptError m) => UserOpts -> m UserInfo
```

### Problems

When they are executed sequentially
```
validateOpts opts >>= parseIpipFile >>= submitBigFive
```
It won't work because they encounter different error types.

Possible solutions:
* Replace the error type by text string.  But it loses the type safety.
* Define the ADT as
```
data AppError = ParseError Text | HttpError Text | OptError Text
```
This doesn't make sense because in, say, `parseIpipFile`, `AppError` type means the error can be `HttpError Text` or `OptError Text` as well, but we know it won't happen in runtime.

### Solution - classy prisms

If we improve `parseIpipFile` by using classy prism
```
parseIpipFile 
  :: (MonadError e m, AsParseError e, MonadIO m) 
  => UserInfo -> m BigFiveResult
```
Now the error type is not a concrete type, but bounded by a type class `AsParseError`.  That means any error type satisfying `AsParseError` will do.  When the functions are executed sequentially
```
validateOpts opts >>= parseIpipFile >>= submitBigFive
```
The error type will be bounded by the following constraints.
```
MonadError e m, MonadReader BotEnv m, MonadIO m, AsOptError e, AsParseError e, AsHttpError e
```

If we define `AppError` that satisfies the error type constraints.  
```
data AppError
  = ParseAppError ParseError
  | HttpAppError HttpError
  | OptAppError OptError
```
Then monad stack `ReaderT BotEnv (ExceptT AppError IO)` solves the problem.

### Note
`ParseError` is by default `AsParseError` instance.  Therefore, if only `parseIpipFile` is called, concrete type `ParseError` is sufficient.

Reference:

* https://www.parsonsmatt.org/2018/11/03/trouble_with_typed_errors.html
* https://carlo-hamalainen.net/2015/07/20/classy-mtl/

## Appendix - Build, test and run the application

### Build the application

* Under the project folder, run `nix-shell`
* In the nix shell, run `cabal build`

### Run the test-suites

* In the nix shell, run `cabal new-test test:tests`

### Run the application

* To check help information, run `cabal run mtl-classy-prism -- --help`

* Run the application according to the help information.

