SELECT c.*, p.inception_date
FROM claims_bdx AS c
LEFT JOIN policy_bdx AS p ON p.policy_ref = c.policy_ref;