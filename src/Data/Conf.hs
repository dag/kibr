-- | Provides the 'Conf' type class.
module Data.Conf
    ( Conf(..)
    )
  where

import qualified Happstack.Server as Happstack
import qualified Network.IRC.Bot  as IRCBot

-- | Data types representing configuration.  Differs in semantics from
-- /data-default/; focus is on sane and useful defaults rather than void
-- values.
class Conf c where
    -- | Default/base configuration.
    conf :: c

instance Conf Happstack.Conf where conf = Happstack.nullConf
instance Conf IRCBot.BotConf where conf = IRCBot.nullBotConf
instance Conf IRCBot.User    where conf = IRCBot.nullUser
