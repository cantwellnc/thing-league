module Web.Model.Core where
import IHP.Prelude

newtype LeagueId = LeagueId UUID
newtype UserId = UserId UUID
newtype RoundId = RoundId UUID
newtype SubmissionId = SubmissionId UUID
newtype VoteId = VoteId UUID
data MemberId = MemberId UUID UUID -- league + user


data LeagueState = LeagueOpen | LeagueInProgress | LeagueComplete deriving (Eq, Show)
data RoundState = Open | Voting | Complete deriving (Eq, Show)

data Round = Round {
    roundId               :: RoundId,
    roundLeague           :: LeagueId,
    roundTheme            :: Text,
    roundState            :: RoundState,
    roundSubmitDeadline   :: UTCTime,
    roundBudget           :: Int,
    roundMaxPerSubmission :: Int
}

data User = User {
    id          :: UserId, 
    displayName :: Text, 
    email       :: Text, 
    createdAt   :: UTCTime
}

data League = League {
    id        :: LeagueId,
    name      :: Text,
    createdBy :: UserId,
    createdAt :: UTCTime
}

data LeagueMember = LeagueMember {
    leagueId :: LeagueId, 
    userId   :: UserId, 
    joinedAt :: UTCTime
}

data Submission = Submission {
    id          :: SubmissionId, 
    url         :: Text, 
    comment     :: Text, 
    submittedAt :: UTCTime
}

data Vote = Vote {
    id      :: VoteId, 
    roundId :: RoundId,
    voterId :: UserId, 
    points  :: Int
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
data RoundEvent = RoundStarted | SubmissionsOpened | SubmissionsClosed | VotingOpened | VotingClosed | RoundEnded

-- placeholders
data LeagueCommand = StartLeague | EndLeague
data RoundCommand = StartRound |  OpenSubmissions | CloseSubmissions | OpenVoting | CloseVoting | EndRound


processLeagueEvent :: LeagueEvent -> League -> League
processLeagueEvent event league = undefined

processRoundEvent :: RoundEvent -> Round -> Round
processRoundEvent event round = undefined

processLeagueCommand :: LeagueCommand -> League -> League
processLeagueCommand command league = undefined

processRoundCommand :: RoundCommand -> Round -> Round
processRoundCommand command round = undefined