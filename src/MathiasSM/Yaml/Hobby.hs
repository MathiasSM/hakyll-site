-- | A hobby, as read from the single hobbies YAML list
module MathiasSM.Yaml.Hobby (
    Hobby (..),
    parseHobby,
    parseHobbiesYaml,
) where

import Data.Aeson (Object)
import Data.Maybe (fromMaybe)
import MathiasSM.Metadata qualified as K
import MathiasSM.Yaml (Check (runCheck), optional, parseYamlList, required)

-- | What a hobby entry needs
data Hobby = Hobby
    { hobbyName :: String
    , hobbyHref :: String
    , hobbyIcon :: String
    , hobbyShow :: Bool
    }
    deriving (Eq, Show)

-- | Parses a hobby, reporting every problem (not just the first)
parseHobby :: Object -> Either [String] Hobby
parseHobby o =
    runCheck
        ( Hobby
            <$> required o K.Name
            <*> required o K.Href
            <*> required o K.Icon
            <*> (fromMaybe True <$> optional o K.Show)
        )

-- | 'parseHobby' on the contents of the hobbies file (a list of hobbies)
parseHobbiesYaml :: String -> Either [String] [Hobby]
parseHobbiesYaml = parseYamlList "hobby" parseHobby
