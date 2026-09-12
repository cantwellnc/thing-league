
module Web.Model.Core where

import IHP.Prelude
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Ord (Down (..))

-- TODO (claude): once this connects to IHP persistence, consider replacing these with
-- IHP.ModelSupport's Id' pattern (newtype Id' table = Id (PrimaryKey table);
-- type Id model = Id' (GetTableName model)) instead of one newtype per id.
newtype LeagueId = LeagueId UUID deriving (Eq, Show)
newtype UserId = UserId UUID deriving (Eq, Show)
newtype RoundId = RoundId UUID deriving (Eq, Show)
newtype SubmissionId = SubmissionId UUID deriving (Eq, Ord, Show)
newtype VoteId = VoteId UUID deriving (Eq, Show)
data MemberId = MemberId UUID UUID -- league + user

type Ranking = [(Submission, Int)] -- (submission, # of points it got)

-- League config (voteBudget, maxVotesPerSubmission) is fixed once and lives on
-- League itself now -- a Round just looks it up via roundLeague, no need to
-- duplicate it here.
data Round
    = OpenedRound
        { roundId :: RoundId
        , roundLeague :: LeagueId
        , roundTheme :: Text
        , mediaSubmitDeadline :: UTCTime
        }
    | VotingRound
        { roundId :: RoundId
        , roundLeague :: LeagueId
        , roundTheme :: Text
        , voteSubmitDeadline :: UTCTime
        }
    | CompletedRound
        { roundId :: RoundId
        , roundLeague :: LeagueId
        , roundTheme :: Text
        , ranking :: Ranking
        }
    deriving Show

data RoundEvent
    = RoundStarted
    | VotingOpened
        { voteSubmitDeadline :: UTCTime
        }
    | RoundCompleted Ranking
    deriving Show

{-
work of summing events, getting data that event processor needs from submissions / rounds, etc.
is packed into the place running processRoundCommand. emits events are "record" of what transpired.
-}

-- have to provide initial constructor for first event
startRound :: RoundId -> LeagueId -> Text -> UTCTime -> Round
startRound roundId leagueId theme submitDeadline 
    = OpenedRound {
        roundId = roundId, 
        roundLeague = leagueId, 
        roundTheme = theme, 
        mediaSubmitDeadline = submitDeadline
    }

processRoundEvent :: RoundEvent -> Round -> Either Text Round
processRoundEvent (VotingOpened voteDeadline) (OpenedRound id league theme submitDeadline) =
    Right $ VotingRound
        { roundId = id
        , roundLeague = league
        , roundTheme = theme
        , voteSubmitDeadline = voteDeadline
        }
processRoundEvent (RoundCompleted ranking) (VotingRound id league theme submitDeadline) =
    Right $ CompletedRound
        { roundId = id
        , roundLeague = league
        , roundTheme = theme
        , ranking = ranking
        }
processRoundEvent event round = Left $ "Illegal transition " <> (tshow event) <> " in round " <> (tshow round)


data RoundCommand
    = OpenVoting
        { voteSubmitDeadline :: UTCTime
        }
    | CompleteRound [Submission] [Vote]
    deriving Show

-- for a round, if command is legal, produce the roundEvent that records the decision
processRoundCommand :: RoundCommand -> Round -> Either Text RoundEvent
processRoundCommand (OpenVoting deadline) (OpenedRound {}) =
    Right (VotingOpened deadline)
processRoundCommand (CompleteRound submissions votes) (VotingRound { roundId = thisRoundId }) =
    Right (RoundCompleted ranking)
  where
    roundSubmissions = filter (\(Submission { roundId = rid }) -> rid == thisRoundId) submissions
    roundVotes = filter (\(Vote { roundId = rid }) -> rid == thisRoundId) votes
    -- points a submission received, summed across every vote for it
    pointsBySubmission :: Map SubmissionId Int
    pointsBySubmission = Map.fromListWith (+) [(sid, pts) | Vote { submissionId = sid, points = pts } <- roundVotes]
    -- submissions with zero votes still appear, ranked last, at 0 points
    ranking =
        roundSubmissions
            |> map (\s@(Submission { id = sid }) -> (s, Map.findWithDefault 0 sid pointsBySubmission))
            |> sortOn (Down . snd)
processRoundCommand command round =
    Left $ "Illegal command " <> tshow command <> " for round " <> tshow round


-- WIP

data User = User
    { id :: UserId
    , displayName :: Text
    , email :: Text
    , createdAt :: UTCTime
    }
    deriving Show

-- final player standings, aggregated across all rounds -- (player, total points), sorted desc
type Standings = [(User, Int)]

data League
    = OpenLeague
        -- everything is fixed except membership; people join via inviteCode
        { leagueId :: LeagueId
        , leagueName :: Text
        , createdBy :: UserId
        , createdAt :: UTCTime
        , inviteCode :: Text
        , voteBudget :: Int
        , maxVotesPerSubmission :: Int
        }
    | InProgressLeague
        -- membership, rounds, and voting structure are all locked in now
        { leagueId :: LeagueId
        , leagueName :: Text
        , createdBy :: UserId
        , createdAt :: UTCTime
        , voteBudget :: Int
        , maxVotesPerSubmission :: Int
        }
    | CompleteLeague
        { leagueId :: LeagueId
        , leagueName :: Text
        , createdBy :: UserId
        , createdAt :: UTCTime
        , standings :: Standings
        }
    deriving Show

-- same reasoning as startRound: creation is not a transition on an existing value
createLeague :: LeagueId -> Text -> UserId -> UTCTime -> Text -> Int -> Int -> League
createLeague leagueId name owner createdAt inviteCode budget maxPerSubmission =
    OpenLeague
        { leagueId = leagueId
        , leagueName = name
        , createdBy = owner
        , createdAt = createdAt
        , inviteCode = inviteCode
        , voteBudget = budget
        , maxVotesPerSubmission = maxPerSubmission
        }

data LeagueMember = LeagueMember
    { leagueId :: LeagueId
    , userId :: UserId
    , joinedAt :: UTCTime
    }

data Submission = Submission
    { id :: SubmissionId
    , submittedBy :: UserId
    , roundId :: RoundId
    , media :: Text
    , comment :: Text
    , submittedAt :: UTCTime
    }
    deriving Show

data Vote = Vote
    { id :: VoteId
    , voterId :: UserId
    , submissionId :: SubmissionId
    , roundId :: RoundId
    , points :: Int
    }
    deriving Show

{-
flow:

create account if not exists
create league if not exists
generate link if not exists

register for league if not registered

-- league begins
-- round 1 begins
-- create submission if not exists
    -- possibly update existing submission
-- submission ends, voting begins
-- create votes if not exists
    -- possibly update existing votes

What functionality do we need?
- CRUD user

- CRUD league
- join league as user
- create invite for league
- league transitions
- results for league

- Round transitions
- create playlist of media for Round
- save vote(s) for Round
- submit vote(s) for Round
- results for Round (calc-ed by counting up votes for each song + assigning 1st/2nd/3rd place in decreasing order of amt of votes)
-}

data LeagueEvent = LeagueStarted | LeagueFinished

-- placeholders
data LeagueCommand = StartLeague | EndLeague

processLeagueEvent :: LeagueEvent -> League -> League
processLeagueEvent event league = undefined

processLeagueCommand :: LeagueCommand -> League -> League
processLeagueCommand command league = undefined
