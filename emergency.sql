/*
This was part of a weekly review to check for long wait times of emergency (911)
calls--which, for obvious reasons, were categorically prioritized--and provide a
starting point to investigate any systemic causes other than, for example, rare
and unpredictable spikes in the number of 911 calls.

At the time that I inherited this review, it had been using a file-based export
from a company-internal web server, generated via a separate query that had
originally been created for a different purpose and was not well-suited to this
review; it included summarization that resulted in a number of false
positives for long wait times, did not itself incorporate checks for call
spikes, and required later mapping of customer categories for the end report.

This query provided a readily-refreshable data pull that was specific to and
accurate for the purpose, vastly reducing the time to complete the review.

Call spikes are checked by number of calls within 1- and 2-minute windows prior
to any given call, achieved in the query via a self-join of QueueActivity.
*/

DECLARE
  --Previous week
  @Start DATETIME = DATEADD(hh,7,DATEADD(wk,DATEDIFF(wk,0,GETDATE())-1,0)),
  @End DATETIME = DATEADD(hh,7,DATEADD(wk,DATEDIFF(wk,0,GETDATE()),0)),
  @Queue INT = 911
 
;WITH
 
--Customer categorization mapping
v AS (
  SELECT
    v.AccountNo,
    c.CustomerGroup
  FROM AccountQueue v
  LEFT JOIN Mapping.dbo.CustomerMapping c on c.CustomerNo = v.CustomerNo
  GROUP BY
    v.AccountNo,
    c.CustomerGroup
),
 
--Self-join for 2-minute window
b AS (
  SELECT
    x.QueueStartTime,
    x.QueueStartId
  FROM QueueActivity x
  WHERE
  x.QueueStartTime >= DATEADD(mi,-2,@Start)
  AND x.QueueStartTime < @End
  AND x.QueueNo = @Queue
)
 
SELECT
  DATEADD(hh,-7,a.QueueStartTime) QueueStartTimeAZ
  ,v.CustomerGroup
  ,a.Callid
  --Flag for calls not answered within a defined threshold:
  ,a.CallsAbandAfterThres + a.CallsAnswAfterThres Missed
  --Wait time unique to each queue instance
  ,a.QueueWaitDuration
  ,DATEADD(hh,-7,DATEADD(mi,DATEDIFF(mi,0,a.QueueStartTime)/15*15,0)) Interval15
  ,SUM(CASE WHEN b.QueueStartTime >= DATEADD(mi,-1,a.queuestart)
       AND b.QueueStartTime <= a.QueueStartTime THEN 1 ELSE 0 END) '1Min'
  ,SUM(CASE WHEN b.QueueStartTime >= DATEADD(mi,-2,a.queuestart)
       AND b.QueueStartTime <= a.QueueStartTime THEN 1 ELSE 0 END) '2Min'
FROM
  QueueActivity a
 
LEFT JOIN v
  ON v.AccountNo = a.AccountNo
 
LEFT JOIN b
  ON b.QueueStartId <> a.QueueStartId
 
WHERE
  a.QueueStartTime >= @Start
  AND a.QueueStartTime < @End
  AND a.Queueno = @Queue
 
GROUP BY
  DATEADD(hh,-7,a.QueueStartTime)
  ,v.CustomerGroup
  ,a.Callid
  ,a.CallsAbandAfterThres + a.CallsAnswAfterThres
  ,a.QueueWaitDuration
  ,dateadd(mi,datediff(mi,0,a.QueueStartTime)/15*15,0)
 
ORDER BY
  DATEADD(hh,-7,a.QueueStartTime)
