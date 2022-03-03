import Ledger "ledger";
import Nat64 "mo:base/Nat64";
import Time "mo:base/Time";
import Int "mo:base/Int";
import Debug "mo:base/Debug";
import AviateAID "mo:principal/blob/AccountIdentifier";
import Ext "mo:ext/Ext";
actor {
    private let ICP_FEE: Nat64 = 10000; // e8s 
    private stable var transferIndex: Nat64 = 0;
    
    public shared(msg) func greet() : async () {
        ignore _sendICP(msg.caller,1000000);
    };
    let ledgerActor : Ledger.Self = actor("ryjl3-tyaaa-aaaaa-aaaba-cai");
    private func _sendICP(to : Principal,amount : Nat64) : async (){
        let res = await ledgerActor.transfer({
            memo = transferIndex;
            from_subaccount = null;
            to =  AviateAID.fromPrincipal(to, null);
            amount = { e8s = amount - ICP_FEE };
            fee = { e8s = ICP_FEE };
            created_at_time = ?{ timestamp_nanos = Nat64.fromNat(Int.abs(Time.now())) };
        });
        Debug.print(debug_show(res));
        switch(res){
            case (#Ok(_)){
                Debug.print(debug_show("OK"));
            };
            case (#Err(_)){
                Debug.print(debug_show("Err"));
            };
        }
    };
};
