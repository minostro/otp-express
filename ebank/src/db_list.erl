-module(db_list).
-record(account, {no,
                 balance=0,
                 pin,
                 name,
                 transactions=[]}).

-export([empty/0, insert/2, db_to_list/1, db_size/1, lookup/2, lookup_all/3, update/2, close/1]).

empty() -> [].

insert(#account{no = No} = Account, DBRef) ->
  case lists:keyfind(No, #account.no, DBRef) of
    false -> [Account | DBRef];
    _Account -> {error, exists}
  end.

db_to_list(DBRef) -> DBRef.

db_size(DBRef) -> length(DBRef).

lookup(AccountNumber, DBRef) -> lists:keyfind(AccountNumber, #account.no, DBRef).

lookup_all(AccountField, Key, DBRef) -> lists:filter(fun(Elem) -> element(AccountField, Elem) =:= Key end, db_to_list(DBRef)).

update(#account{no = No} = Account, DBRef) -> lists:keyreplace(No, #account.no, DBRef, Account).

close(_DBRef) -> ok.
