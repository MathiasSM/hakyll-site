-- | A showcase project's fields as a template context
module MathiasSM.Context.Project (projectContext) where

import Hakyll (Context, Item (itemBody), field, noResult)
import MathiasSM.Context.Language (languageContext)
import MathiasSM.Domain.Project (Project (..), statusName)

-- | A showcase project's fields, as read by the project templates
projectContext :: Context Project
projectContext =
    mconcat
        [ projectField "title" projectTitle
        , projectField "href" projectHref
        , projectField "status" (statusName . projectStatus)
        , projectField "startDate" (show . projectStart)
        , optionalField "endDate" (fmap show . projectEnd)
        , projectField "shortDescription" projectShortDescription
        , projectField "longDescription" projectLongDescription
        , languageContext
        ]
  where
    projectField name get = field name (return . get . itemBody)
    optionalField name get = field name (maybe (noResult $ "No " ++ name) return . get . itemBody)
