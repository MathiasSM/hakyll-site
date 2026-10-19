-- | An experience item, as read from the single experience YAML list
module MathiasSM.Yaml.Experience (
    Experience (..),
    parseExperience,
    parseExperiencesYaml,
) where

import Data.Aeson (Object)
import MathiasSM.Metadata qualified as K
import MathiasSM.Yaml (Check (runCheck), parseYamlList, required)

-- | What an experience item needs to be listed
data Experience = Experience
    { experienceIcon :: String
    , experienceName :: String
    }
    deriving (Eq, Show)

-- | Parses an experience item, reporting every problem (not just the first)
parseExperience :: Object -> Either [String] Experience
parseExperience o =
    runCheck
        ( Experience
            <$> required o K.Icon
            <*> required o K.Name
        )

-- | 'parseExperience' on the contents of the experience file (a list of items)
parseExperiencesYaml :: String -> Either [String] [Experience]
parseExperiencesYaml = parseYamlList "experience" parseExperience
