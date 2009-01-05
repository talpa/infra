select
  id,
  accountnumber,
  accountname as name,
  initialbalance,
  currentbalance
from account
where
  id=:id
