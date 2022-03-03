[How to deploy ledger locally](https://github.com/dfinity/ic/tree/master/rs/rosetta-api/ledger_canister#deploying-locally)

update ledger canister Id in main.mo

```bash
let ledgerActor : Ledger.Self = actor("rrkah-fqaaa-aaaaa-aaaaq-cai");
```

Test:
```bash
vessel install
dfx start
dfx deploy
dfx canister call mysite greet
```