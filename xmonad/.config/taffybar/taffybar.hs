--
--  File:       taffybar.hs
--  Author:     Juan Pedro Bolívar Puente <raskolnikov@es.gnu.org>
--  Date:       Sat Jan  3 15:00:03 2015
--
--  Taffybar configuration file. Hola. Amigo.
--

import System.Taffybar
import System.Taffybar.SimpleConfig

import System.Taffybar.Widget
import System.Taffybar.Widget.Workspaces
import System.Taffybar.Widget.Battery
import System.Taffybar.Widget.Generic.PollingBar
import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Widget.Generic.VerticalBar
import System.Taffybar.Widget.MPRIS2
import System.Taffybar.Widget.SNITray
import System.Taffybar.Widget.SimpleClock
import System.Taffybar.Widget.Util

import System.Taffybar.Information.Memory
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Battery

import Data.Text
import Text.Printf

import GI.Gtk (labelNew, labelSetMarkup, widgetShowAll, toWidget)

main :: IO ()
main = do
  let bgc = 0
  let bg = (bgc, bgc, bgc, 1)
  let fg = (0.43, 0.43, 0.43, 1)
  let darkText = "#444"
  let lightText = "#aaa"
  let selected = "#BD93F9"

  let memCallback = do
        mi <- parseMeminfo
        return [memoryUsedRatio mi]

      cpuCallback = do
        (_, systemLoad, totalLoad) <- cpuLoad
        return [totalLoad, systemLoad]

      label str color = do
        l <- labelNew (Nothing :: Maybe Text)
        labelSetMarkup l $ pack $ (colorize color "" str)
        widgetShowAll l
        toWidget l

  let memCfg = defaultGraphConfig
        { graphDataColors = [ fg ]
        , graphLabel = Just $ pack $ colorize darkText "" ""
        , graphBackgroundColor = bg
        , graphBorderColor = bg
        , graphPadding = 0
        , graphBorderWidth = 0
        }
      cpuCfg = defaultGraphConfig
        { graphDataColors = [ fg
                            , fg
                            ]
        , graphLabel = Just $ pack $ colorize darkText "" ""
        , graphBackgroundColor = bg
        , graphBorderColor = bg
        , graphPadding = 0
        , graphBorderWidth = 0
        }

  let clock = textClockNew Nothing (colorize selected "" "%a %b %_d %H:%M") 1
      workspaces = workspacesNew defaultWorkspacesConfig
        { minIcons = 0
        , maxIcons = Just 0
        , widgetGap = 4
        }
      layout = layoutNew defaultLayoutConfig
      windowsW = windowsNew defaultWindowsConfig
      mpris = mpris2New
      bat = textBatteryNew "$percentage$"
      bat' = label "   BAT  " darkText
      mem = pollingGraphNew memCfg 1 memCallback
      mem' = label "   MEM  " darkText
      cpu = pollingGraphNew cpuCfg 1 cpuCallback
      cpu' = label "   CPU  " darkText
      tray = sniTrayNew
      sep = label "   ╱   " darkText
      sep' = label "  " ""
      sep'' = label "   " ""

  simpleTaffybar defaultSimpleTaffyConfig
    { barHeight = ExactSize 26
    , widgetSpacing = 0
    , startWidgets = [ sep', workspaces, sep, layout, sep, windowsW ]
    , endWidgets = [ sep'', clock, sep, tray, sep, bat, bat', mem, mem', cpu, cpu', sep, mpris, sep'' ]
    , barPosition = Top
    , System.Taffybar.SimpleConfig.barPadding = 0
    }
