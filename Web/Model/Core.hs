
module Web.Model.Core where

import IHP.Prelude

-- TODO (claude): once this connects to IHP persistence, consider replacing these with
-- IHP.ModelSupport's Id' pattern (newtype Id' table = Id (PrimaryKey table);
-- type Id model = Id' (GetTableName model)) instead of one newtype per id.
newtype LeagueId = LeagueId UUID deriving (Eq, Show)
newtype UserId = UserId UUID deriving (Eq, Show)
newtype RoundId = RoundId UUID deriving (Eq, Show)
newtype SubmissionId = SubmissionId UUID deriving (Eq, Show)
newtype VoteId = VoteId UUID deriving (Eq, Show)
data MemberId = MemberId UUID UUID -- league + user

type Ranking = [(Submission, Int)] -- (submission, # of points it got)
data LeagueState
    = LeagueOpen
    | LeagueInProgress
    | LeagueComplete
    deriving (Eq, Show)

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
        , voteBudget :: Int
        , maxVotesPerSubmission :: Int
        }
    | CompletedRound
        { roundId :: RoundId
        , roundLeague :: LeagueId
        , roundTheme :: Text
        , ranking :: Ranking
        }
    deriving Show
    

data RoundCommand
    = StartRound
    | OpenVoting
    | CompleteRound

data RoundEvent
    = RoundStarted
    | VotingOpened
        { voteSubmitDeadline :: UTCTime
        , voteBudget :: Int
        , maxVotesPerSubmission :: Int
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
processRoundEvent (VotingOpened voteDeadline voteBudget maxVotes) (OpenedRound id league theme submitDeadline) =
    Right $ VotingRound
        { roundId = id
        , roundLeague = league
        , roundTheme = theme
        , voteSubmitDeadline = submitDeadline
        , voteBudget = voteBudget
        , maxVotesPerSubmission = maxVotes
        }
processRoundEvent (RoundCompleted ranking) (VotingRound id league theme submitDeadline _ _) =
    Right $ CompletedRound
        { roundId = id
        , roundLeague = league
        , roundTheme = theme
        , ranking = ranking
        }
processRoundEvent event round = Left $ "Illegal transition " <> (tshow event) <> " in round " <> (tshow round)

-- WIP

data User = User
    { id :: UserId
    , displayName :: Text
    , email :: Text
    , createdAt :: UTCTime
    }

data League = League
    { id :: LeagueId
    , name :: Text
    , createdBy :: UserId
    , createdAt :: UTCTime
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

processRoundCommand :: RoundCommand -> Round -> Round
processRoundCommand command round = undefined
