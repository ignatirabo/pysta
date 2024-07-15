db = sqlite_utils.db.Database("my_database.db")
policy = input()
statement = f"select policy_name from policies where policy_name='{policy}' and public='True'"
q = db.query(statement)
# q: list[int] = db.query(statement)
# print(policy)
