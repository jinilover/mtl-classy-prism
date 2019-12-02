-- | Types and functions to bootstrap the functions
module Bootstrap 
  ( bootstrap )
  where

import Control.Lens
import Control.Monad.Except     (liftEither)
import Options.Applicative      (Parser, execParser, info, helper, help, fullDesc, progDesc, header, strOption, long, metavar)
import Text.Email.Validate      (validate)

import Bot.Client
import Bot.Types
import Config
import Ipip.Parsers
import Types

-- | Bootstrap the application
-- It parses the command line argument for the user name, user email and 
-- location of the test result html file,
-- loads API endpoint configured in the configuration file,
-- parses the html file for the facet scores and send the result to the API.
bootstrap :: IO ()
bootstrap = do
  opts@UserOpts{..} <- execParser . flip info desc $ helper <*> userOptionsParser
  env <- loadBotConfig >>= mkBotEnv
  print ("Parsing " <> testResultLocation <> ", submitting the result for " <> userName <> " " <> userEmailText)
  response <- (runExceptT . flip runReaderT env) (validateOpts opts >>= parseIpipFile >>= submitBigFive)
  either handleErr success response
  where
    handleErr :: AppError -> IO ()
    handleErr (ParseAppError (IpipParseError msg)) = print ("Error in parsing file: " <> msg)
    handleErr (HttpAppError (HttpClientError err)) = print ("Http error in sending data to Recruitbot API: " <> (show err :: Text))
    handleErr (HttpAppError _) = print ("Http 404 error, please check the uri path of result submission" :: Text)
    handleErr (OptAppError (OptParseError msg)) = print ("Invalid --email: " <> msg)
    success :: Text -> IO ()
    success token = print ("Successfully submitted result with replied token: " <> token)
    desc = fullDesc 
            <> progDesc "Parse and submit USER test result to Recruitbot service"
            <> header "Welcome to submit test result to RecruitBot"

-- | For parsing the command line arguments
userOptionsParser :: Parser UserOpts
userOptionsParser = UserOpts <$> name <*> email <*> ipipFile
  where
    name = strOption
            ( long "username"
            <> metavar "USER"
            <> help "User name, e.g. Lambda")
    email = strOption
            ( long "email"
            <> metavar "EMAIL"
            <> help "User email, e.g. lambda@category.org")
    ipipFile = strOption
                ( long "ipipFile"
                <> metavar "FILE"
                <> help "Test result location, e.g. /home/test/big5result")

-- | Validate the command line argument of user email to make sure
-- it has valid email address format.
validateOpts :: (MonadError e m, AsOptError e) => UserOpts -> m UserInfo
validateOpts UserOpts{..} = 
  let either' = validate (toS userEmailText) <&> UserInfo userName
                                             <*> pure testResultLocation
  in  liftEither . first ((_OptParseError #) . toS) $ either'

data UserOpts = 
  UserOpts
    { userName :: Text
    , userEmailText :: Text
    , testResultLocation :: Text }
