{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Bits ( (.&.), shiftR )
import           Data.Word ( Word8, Word16, Word32 )
import           System.IO

import           Nes.Audio ( pcmBytes, samplesOf )
import           Nes.Bus ( busApu, busCart, busMap, busSetController )
import           Nes.Cart ( Cart(..), mapperName, parseINES, prgSize, chrSize )
import           Nes.Demo ( demoCart, demoMachine )
import           Nes.Machine ( Machine(..), newMachine, runFrame )
import           Nes.Mapper ( mapperReadPrg )

-- | Exact 61,440-byte video buffer (scanlines 0-239, 256 pixels each).
frameBytes :: Machine -> BS.ByteString
frameBytes m =
  let rows = reverse (mRows m)
      packed = BS.concat (map BS.pack rows)
      padLen = 61440 - BS.length packed
  in if padLen <= 0 then BS.take 61440 packed else packed <> BS.replicate padLen 0

-- | Big-endian 32-bit Word.
putWord32BE :: Word32 -> BS.ByteString
putWord32BE w = BS.pack
  [ fromIntegral ((w `shiftR` 24) .&. 0xFF)
  , fromIntegral ((w `shiftR` 16) .&. 0xFF)
  , fromIntegral ((w `shiftR` 8) .&. 0xFF)
  , fromIntegral (w .&. 0xFF)
  ]

-- | Big-endian 16-bit Word.
putWord16BE :: Word16 -> BS.ByteString
putWord16BE w = BS.pack
  [ fromIntegral ((w `shiftR` 8) .&. 0xFF)
  , fromIntegral (w .&. 0xFF)
  ]

readWord32BE :: Handle -> IO Word32
readWord32BE h = do
  bs <- BS.hGet h 4
  if BS.length bs < 4
    then pure 0
    else do
      let b0 = fromIntegral (BS.index bs 0) :: Word32
          b1 = fromIntegral (BS.index bs 1) :: Word32
          b2 = fromIntegral (BS.index bs 2) :: Word32
          b3 = fromIntegral (BS.index bs 3) :: Word32
      pure ((b0 `shiftR` (-24)) + (b1 `shiftR` (-16)) + (b2 `shiftR` (-8)) + b3)
  where
    -- shiftL is better
    shiftL32 x s = fromIntegral x * (2 ^ s)

cartInfoJson :: Cart -> String
cartInfoJson c =
  "{\"mapper\":\"" ++ mapperName c ++ "\",\"mapperNum\":" ++ show (cartMapper c)
  ++ ",\"mirror\":\"" ++ show (cartMirroring c) ++ "\",\"prgSize\":" ++ show (prgSize c)
  ++ ",\"chrSize\":" ++ show (chrSize c) ++ ",\"battery\":" ++ (if cartBattery c then "true" else "false")
  ++ "}"

mainLoop :: Machine -> Cart -> IO ()
mainLoop !m !currentCart = do
  cmd <- BS.hGet stdin 1
  if BS.null cmd
    then pure ()
    else case C8.head cmd of
      'F' -> do
        padByte <- BS.hGet stdin 1
        let pad = if BS.null padByte then 0 else BS.index padByte 0
        let m' = runFrame (m { mBus = busSetController 0 pad (mBus m) })
        let !video = frameBytes m'
        let apu = busApu (mBus m')
        let readPrg = mapperReadPrg (busMap (mBus m'))
        let !audio = pcmBytes (samplesOf readPrg 744 apu)
        BS.hPut stdout (C8.pack "NES1")
        BS.hPut stdout (putWord32BE (fromIntegral (BS.length video)))
        BS.hPut stdout (putWord32BE (fromIntegral (BS.length audio)))
        BS.hPut stdout video
        BS.hPut stdout audio
        hFlush stdout
        let !mNext = m' { mRows = [] }
        mainLoop mNext currentCart

      'R' -> do
        let m' = newMachine currentCart
        BS.hPut stdout (C8.pack "OK")
        hFlush stdout
        mainLoop m' currentCart

      'D' -> do
        let m' = demoMachine
        BS.hPut stdout (C8.pack "OK")
        hFlush stdout
        mainLoop m' demoCart

      'L' -> do
        lenBytes <- BS.hGet stdin 4
        if BS.length lenBytes < 4
          then do
            BS.hPut stdout (C8.pack "ER")
            hFlush stdout
            mainLoop m currentCart
          else do
            let b0 = fromIntegral (BS.index lenBytes 0) :: Int
                b1 = fromIntegral (BS.index lenBytes 1) :: Int
                b2 = fromIntegral (BS.index lenBytes 2) :: Int
                b3 = fromIntegral (BS.index lenBytes 3) :: Int
                len = (b0 * 16777216) + (b1 * 65536) + (b2 * 256) + b3
            romData <- BS.hGet stdin len
            case parseINES romData of
              Left err -> do
                let errBS = C8.pack err
                BS.hPut stdout (C8.pack "ER")
                BS.hPut stdout (putWord16BE (fromIntegral (BS.length errBS)))
                BS.hPut stdout errBS
                hFlush stdout
                mainLoop m currentCart
              Right newCart -> do
                let m' = newMachine newCart
                BS.hPut stdout (C8.pack "OK")
                hFlush stdout
                mainLoop m' newCart

      'I' -> do
        let info = C8.pack (cartInfoJson currentCart)
        BS.hPut stdout (C8.pack "IN")
        BS.hPut stdout (putWord16BE (fromIntegral (BS.length info)))
        BS.hPut stdout info
        hFlush stdout
        mainLoop m currentCart

      'Q' -> pure ()
      _   -> mainLoop m currentCart

main :: IO ()
main = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  mainLoop demoMachine demoCart
