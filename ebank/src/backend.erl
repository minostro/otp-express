-module(backend).
-include("../include/backend.hrl").

-behaviour(gen_server).

-define(DB, db_list).
-define(ACCOUNTS,
        [{1, 100, "1234", "Henry Nystrom"},
         {2, 200, "4321", "Francesco Cesarini"},
         {3, 1000, "1111", "Donald Duck"},
         {4, 5000, "1234", "Henry Nystrom"}
        ]).

%% API
-export([start/0,
         stop/0,
         start_link/0,
         pin_valid/2,
         change_pin/3,
         account/1,
         balance/2,
         transactions/2,
         withdraw/3,
         transfer/4,
         deposit/2,
         block/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {accounts, blocked = []}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], [{debug, [trace]}]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> ?MODULE ! stop.

account(Account) -> call({account, Account}).

pin_valid(AccountNo, Input) -> call({pin_valid, AccountNo, Input}).

change_pin(User, OldPin, NewPin) -> call({change_pin, User, OldPin, NewPin}).

withdraw(AccountNo, Pin, Amount) -> call({withdraw, AccountNo, Pin, Amount}).

deposit(AccountNo, Amount) -> call({deposit, AccountNo, Amount}).

transfer(Amount, From, To, Pin) -> call({transfer, From, To, Pin, Amount}).

balance(AccountNo, Pin) -> call({balance, AccountNo, Pin}).

transactions(AccountNo, Pin) -> call({transactions, AccountNo, Pin}).

block(AccountNumber) -> call({block, AccountNumber}).

call(Message) ->
  gen_server:call(?MODULE, Message).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  Accounts =
  lists:foldl(fun({No, Balance, Pin, Name}, DB) ->
                  ?DB:insert(new_account(No, Balance, Pin, Name), DB)
              end,
              ?DB:empty(),
              ?ACCOUNTS),
  {ok, #state{accounts = Accounts}}.

handle_call({account, Accounts}, _From, State) ->
  Reply =
  case Accounts of
    all ->
      lists:map(fun(#account{no = No, name = Name}) -> {No, Name} end,
                ?DB:db_to_list(State#state.accounts));
    Name when is_list(Name) -> find_account(Name, State);
    No when is_integer(No) -> [find_account(No, State)]
  end,
  {reply, Reply, State};
handle_call({pin_valid, AccountNumber, Pin}, _From, State) ->
  Account = find_account(AccountNumber, State),
  {reply, do_pin_valid(Account, Pin), State};
handle_call({new_account, [Balance, Pin, Name]}, _From, State) ->
  Accounts = State#state.accounts,
  No = ?DB:db_size(Accounts) + 1,
  NewAccounts = ?DB:insert(new_account(No, Balance, Pin, Name), Accounts),
  {reply, ok, State#state{accounts = NewAccounts}};
handle_call({balance, AccountN, Pin}, _From, State) ->
  {reply, do_balance(AccountN, Pin, State), State};
handle_call({transactions, AccountN, Pin}, _From, State) ->
  {reply, do_transactions(AccountN, Pin, State), State};
handle_call({withdraw, FromAccountN, Pin, Amount}, _From, State) ->
  {Message, State} = case do_withdraw(FromAccountN, Pin, Amount, State) of
    {ok, NewState} -> {ok, NewState};
    {error, Reason} -> {{error, Reason}, State}
  end,
  {reply, Message, State};
handle_call({deposit, ToAccountN, Amount}, _From, State) ->
  {Message, State} = case do_deposit(ToAccountN, Amount, State) of
    {ok, NewState} -> {ok, NewState};
    {error, Reason} -> {{error, Reason},State}
  end,
  {reply, Message, State};
handle_call({transfer, FromAccountN, ToAccountN, Pin, Amount}, _From, State) ->
  {Message, State} = case do_transfer(FromAccountN, ToAccountN, Pin, Amount, State) of
    {ok, NewState} -> {ok, NewState};
    {error, Reason} -> {{error, Reason}, State}
  end,
  {reply, Message, State};
handle_call({change_pin, User, OldPin, NewPin}, _From, State) ->
  {Message, State} = case do_change_pin(User, OldPin, NewPin, State) of
    {ok, NewState} -> {ok, NewState};
    {error, Reason} -> {{error, Reason}, State}
  end,
  {reply, Message, State};
handle_call({block, AccountNumber}, _From, State) ->
  NewState = block(AccountNumber, State),
  {reply, ok, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
new_account(No, Balance, Pin, Name) ->
    #account{no = No, balance = Balance, pin = Pin, name = Name}.

find_account(AccountN, State) when is_integer(AccountN) ->
  ?DB:lookup(AccountN, State#state.accounts);
find_account(User, State) when is_list(User) ->
  ?DB:lookup_all(#account.name, User, State#state.accounts).

do_withdraw(_, _, Amount, _) when Amount < 0 -> {error, "Negative value"};
do_withdraw(AccountN, Pin, Amount, State) ->
  Account = #account{balance = OldBalance, transactions = OldTransactions} =
    find_account(AccountN, State),
  case do_pin_valid(Account, Pin) of
    false -> {error, "PIN code not valid!"};
    true when OldBalance < Amount -> {error, "Not enough money on account!"};
    true ->
      NewBalance = OldBalance - Amount,
      NewTransactions = [{withdraw, date(), Amount} | OldTransactions],
      AccountUpdated =
	Account#account{balance = NewBalance, transactions = NewTransactions},
      NewAccounts = ?DB:update(AccountUpdated, State#state.accounts),
      {ok, State#state{accounts = NewAccounts}}
  end.

do_deposit(AccountN, Amount, State) ->
  Account = #account{balance = OldBalance, transactions = OldTransactions} =
    find_account(AccountN, State),
  NewBalance = OldBalance + Amount,
  NewTransactions = [{deposit, date(), Amount} | OldTransactions],
  AccountUpdated =
    Account#account{balance = NewBalance, transactions = NewTransactions},
  NewAccounts = ?DB:update(AccountUpdated, State#state.accounts),
  {ok, State#state{accounts = NewAccounts}}.

do_balance(AccountN, Pin, State) ->
  Account = find_account(AccountN, State),
  case do_pin_valid(Account, Pin) of
    true -> account_balance(Account, State);
    false -> {error, "PIN code not valid!"}
  end.

account_balance(Account, State) ->
  case is_block(Account#account.no, State) of
    true -> 0;
    false -> Account#account.balance
  end.

do_transactions(AccountN, Pin, State) ->
  Account = find_account(AccountN, State),
  case do_pin_valid(Account, Pin) of
    true -> Account#account.transactions;
    false -> {error, "PIN code not valid!"}
  end.

do_transfer(FromAccountN, ToAccountN, Pin, Amount, State) ->
  case do_withdraw(FromAccountN, Pin, Amount, State) of
    {ok, NewState} -> do_deposit(ToAccountN, Amount, NewState);
    {error, Reason} -> {error, Reason}
  end.

do_pin_valid([], _) -> false;
do_pin_valid([Account | _], Pin) -> Account#account.pin == Pin;
do_pin_valid(Account, Pin) -> Account#account.pin == Pin.

do_change_pin(User, OldPin, NewPin, State) ->
  Accounts = find_account(User, State),
  case do_pin_valid(Accounts, OldPin) of
    false -> {error, "Wrong Pin"};
    true ->
      Accounts1 =
        lists:foldl(fun(Account, Acc) ->
                        ?DB:update(Account#account{pin = NewPin}, Acc)
                    end,
                    State#state.accounts,
                    Accounts),
      {ok, State#state{accounts = Accounts1}}
  end.

block(AccountNumber, #state{accounts = _Accounts} = State) ->
  BlockedAccounts = State#state.blocked,
  State#state{blocked = [AccountNumber | BlockedAccounts]}.

is_block(AccountNumber, #state{blocked = BlockedAccounts}) ->
  lists:any(fun(Ele) -> AccountNumber =:= Ele end, BlockedAccounts).
