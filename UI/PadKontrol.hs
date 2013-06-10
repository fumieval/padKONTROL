{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes #-}
module UI.PadKontrol (PadKontrol, runPadKontrol, module UI.PadKontrol.Types, module Control.Monad.IO.Class) where

import qualified System.MIDI as MIDI
import UI.PadKontrol.Types
import Control.Applicative
import Control.Monad.Operational.Mini
import Control.Monad.IO.Class
import Data.Word
import qualified Data.Map as M
import Data.Char

type PadKontrol = Program Message

instance MonadIO PadKontrol where
    liftIO = singleton . LiftIO

showHex :: Word8 -> String
showHex n = intToDigit (fromEnum $ n `div` 16) : intToDigit (fromEnum $ n `mod` 16) : ""

convertEvent :: MIDI.MidiEvent -> Maybe (Int, Event)
convertEvent (MIDI.MidiEvent time (MIDI.SysEx msg)) = fmap ((,) (fromIntegral time)) $ case drop 4 msg of
    [0x47, 0x00, 0x00] -> Just PedalUp
    [0x47, 0x00, 0x7F] -> Just PedalDown
    [0x45,s,t]
        | s <= 0x0F -> Just $ PadUp (toEnum $ fromEnum s)
        | s >= 0x40 -> Just $ PadDown (toEnum $ fromEnum (s - 0x40)) (fromIntegral (t - 0x30))
    [0x48,0x20,0x7f] -> Just XYTouch
    [0x48,0x20,0x00] -> Just XYRelease
    [0x48,s,0x7f] -> Just $ ButtonDown $ toEnum $ fromEnum s
    [0x48,s,0x00] -> Just $ ButtonUp $ toEnum $ fromEnum s
    [0x49,0x00,v] -> Just $ Knob1 (fromIntegral v / 127)
    [0x49,0x01,v] -> Just $ Knob2 (fromIntegral v / 127)
    [0x4B,x,y] -> Just $ XYPad (fromIntegral x / 127) (fromIntegral y / 127)
    [0x43,0x00,0x01] -> Just JogCW
    [0x43,0x00,0x7F] -> Just JogCCW
    [0x40,0x00,_] -> Nothing
    [0x5f,_,_] -> Nothing
    ev -> error $ "unknown message: " ++ show ev ++ " Please report this as a bug"
convertEvent ev = error $ "unknown event: " ++ show ev

runPadKontrol :: (Int -> Event -> Program Message ()) -> ((forall r. Program Message r -> IO r) -> IO a) -> IO a
runPadKontrol handle m = do
    let g getName = fmap M.fromList . mapM (liftA2 (,) <$> getName <*> return)
    srcs <- MIDI.enumerateSources >>= g MIDI.getName
    dests <- MIDI.enumerateDestinations >>= g MIDI.getName
    let devSrc = srcs M.! "padKONTROL PORT A"
        devDest = dests M.! "padKONTROL CTRL"
    dest <- MIDI.openDestination devDest
    src <- MIDI.openSource devSrc $ Just
        $ interpret (eval dest) . maybe (return ()) (uncurry handle) . convertEvent
    MIDI.start dest
    MIDI.start src
    MIDI.sendSysEx dest $ [0x42, 0x49, 0x6E, 0x08, 0x00, 0x00, 0x01]
    MIDI.sendSysEx dest $ [0x42, 0x49, 0x6E, 0x08, 0x3F, 0x0A, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x29, 0x29, 0x29]
    MIDI.sendSysEx dest $
        [ 0x42, 0x49, 0x6E, 0x08, 0x3F, 0x2A, 0x00, 0x00
        , 0x05, 0x05, 0x05, 0x7F, 0x7E, 0x7F, 0x7F, 0x03
        , 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A
        , 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A
        , 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08
        , 0x09, 0x0A, 0x0B, 0x0C, 0x0d, 0x0E, 0x0F, 0x10]
    result <- m $ interpret (eval dest)
    MIDI.sendSysEx dest $ [0x42, 0x40, 0x6E, 0x08, 0x00, 0x00, 0x00]
    MIDI.stop src
    MIDI.stop dest
    MIDI.close src
    MIDI.close dest
    return result
    where
        light :: Light -> Word8
        light Off = 0x00
        light On = 0x20
        light Blink = 0x63
        light (Flash f) = 0x41 + floor (f * (0x5f - 0x41))

        eval :: MIDI.Connection -> Message a -> IO a
        eval dest (Display x y z) = send dest [0x22, 0x04, 0x00, x, y, z]
        eval dest (DisplayBlink x y z) = send dest [0x22, 0x04, 0x01, x, y, z]
        eval dest (DisplayLeft seg l) = send dest [0x01, 0xB8 + fromIntegral (fromEnum seg), light l]
        eval dest (DisplayCenter seg l) = send dest [0x01, 0xB0 + fromIntegral (fromEnum seg), light l]
        eval dest (DisplayRight seg l) = send dest [0x01, 0xA8 + fromIntegral (fromEnum seg), light l]
        eval dest (PadLight p l) = send dest [0x01, 0x00 + fromIntegral (fromEnum p), light l]
        eval dest (ButtonLight b l) = send dest [0x01, 0x10 + fromIntegral (fromEnum b), light l]
        eval _ (LiftIO m) = m
        eval dest (AllLight w x y z) = send dest [0x3F, 0x0A, 0x01, g0, g1, g2, g3, g4, 0x00, x, y, z]
        　　where
            g = foldr (flip $ flip (+) . (*2)) 0 . map (toEnum . fromEnum . w) 
            g0 = g $ map Left [Pad01 .. Pad07]
            g1 = g $ map Left [Pad08 .. Pad14]
            g2 = g $ map Left [Pad15 .. Pad16] ++ map Right [ButtonScene ..ButtonMidiCh]
            g3 = g $ map Right [ButtonSWType ..ButtonX]
            g4 = g $ map Right [ButtonY .. ButtonHold]


        send dest = MIDI.sendSysEx dest . ([0x42, 0x40, 0x6E, 0x08]++)
