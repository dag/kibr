-- | Provides the 'Configurable' type class.
module Data.Configurable
    ( Configurable(..)
    )
  where

import Happstack.Server (Conf, nullConf)
import Network.IRC.Bot  (BotConf, User, nullBotConf, nullUser)

-- | Data types representing configuration.  Differs in semantics from
-- /data-default/; focus is on sane and useful defaults rather than void
-- values.
class Configurable c where
    -- | Default/base configuration.
    conf :: c

instance Configurable Conf    where conf = nullConf
instance Configurable BotConf where conf = nullBotConf
instance Configurable User    where conf = nullUser
