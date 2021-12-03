{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NumericUnderscores         #-}

module Market.Onchain
    ( apiBuyScript
    , buyScriptAsShortBs
    , typedBuyValidator
    , Sale
    , buyValidator
    , nftDatum
    ) where

import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Codec.Serialise          ( serialise )

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import qualified PlutusTx
import PlutusTx.Prelude
import PlutusTx.Ratio
import Ledger
    ( TokenName,
      PubKeyHash(..),
      CurrencySymbol,
      DatumHash,
      Datum(..),
      txOutDatum,
      txSignedBy,
      ScriptContext(scriptContextTxInfo),
      TxInfo,
      TxInInfo(..),
      txInfoInputs,
      txOutDatumHash,
      Validator,
      TxOut,
      txInfoSignatories,
      unValidatorScript, valuePaidTo )
import qualified Ledger.Typed.Scripts      as Scripts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Ledger.Value              as Value ( valueOf )
import qualified Plutus.V1.Ledger.Ada as Ada (fromValue, Ada (getLovelace))

import           Market.Types               (NFTSale(..), SaleAction(..))


{-# INLINABLE nftDatum #-}
nftDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe NFTSale
nftDatum o f = do
    dh <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE ensureOnlyOneScriptInput #-}
ensureOnlyOneScriptInput :: ScriptContext -> Bool
ensureOnlyOneScriptInput ctx =
  let
    isScriptInput :: TxInInfo -> Bool
    isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
      Nothing -> False
      Just _ -> True
  in if length (filter isScriptInput $ txInfoInputs (scriptContextTxInfo ctx)) <= 1
       then True
       else False

{-# INLINABLE mkBuyValidator #-}
mkBuyValidator :: PubKeyHash -> NFTSale -> SaleAction -> ScriptContext -> Bool
mkBuyValidator pkh nfts r ctx =
    case r of
        Buy   -> traceIfFalse "NFT not sent to buyer" checkNFTOut &&
                 traceIfFalse "Seller not paid" checkSellerOut &&
                 traceIfFalse "Fee not paid" checkMarketplaceFee &&
                 traceIfFalse "Royalities not paid" checkRoyaltyFee &&
                 traceIfFalse "More than one script input" onlyOneScriptInput
        Close -> traceIfFalse "No rights to perform this action" checkCloser
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    tn :: TokenName
    tn = nToken nfts

    cs :: CurrencySymbol
    cs = nCurrency nfts

    seller :: PubKeyHash
    seller = nSeller nfts

    sig :: PubKeyHash
    sig = case txInfoSignatories info of
            [pubKeyHash] -> pubKeyHash
            _ -> error ()

    price :: Integer
    price = nPrice nfts

    checkNFTOut :: Bool
    checkNFTOut = valueOf (valuePaidTo info sig) cs tn == 1

    marketplacePercent :: Integer
    marketplacePercent = 20

    marketplaceFee :: Ratio Integer
    marketplaceFee = max (1_000_000 % 1) (marketplacePercent % 1000 * fromInteger price)

    checkMarketplaceFee :: Bool
    checkMarketplaceFee
      = fromInteger (Ada.getLovelace (Ada.fromValue (valuePaidTo info pkh)))
      >= marketplaceFee

    royaltyFee :: Ratio Integer
    royaltyFee = max (1_000_000 % 1) (nRoyaltyPercent nfts % 1000 * fromInteger price)

    checkRoyaltyFee :: Bool
    checkRoyaltyFee = if nRoyaltyPercent nfts > 0
      then fromInteger (Ada.getLovelace (Ada.fromValue (valuePaidTo info $ nRoyalty nfts))) >= royaltyFee
      else True

    checkSellerOut :: Bool
    checkSellerOut
      =  fromInteger (Ada.getLovelace (Ada.fromValue (valuePaidTo info seller)))
      >= ((fromInteger price - marketplaceFee) - royaltyFee)

    checkCloser :: Bool
    checkCloser = txSignedBy info seller

    onlyOneScriptInput :: Bool
    onlyOneScriptInput = ensureOnlyOneScriptInput ctx


data Sale
instance Scripts.ValidatorTypes Sale where
    type instance DatumType Sale    = NFTSale
    type instance RedeemerType Sale = SaleAction


typedBuyValidator :: PubKeyHash -> Scripts.TypedValidator Sale
typedBuyValidator pkh = Scripts.mkTypedValidator @Sale
    ($$(PlutusTx.compile [|| mkBuyValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode pkh)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @NFTSale @SaleAction


buyValidator :: PubKeyHash -> Validator
buyValidator = Scripts.validatorScript . typedBuyValidator

buyScript :: PubKeyHash -> Plutus.Script
buyScript = Ledger.unValidatorScript . buyValidator

buyScriptAsShortBs :: PubKeyHash -> SBS.ShortByteString
buyScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . buyScript

apiBuyScript :: PubKeyHash -> PlutusScript PlutusScriptV1
apiBuyScript = PlutusScriptSerialised . buyScriptAsShortBs
