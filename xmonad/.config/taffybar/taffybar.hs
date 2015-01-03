--
--  File:       taffybar.hs
--  Author:     Juan Pedro Bolívar Puente <raskolnikov@es.gnu.org>
--  Date:       Sat Jan  3 15:00:03 2015
--
--  Taffybar configuration file.
--

import System.Taffybar
import System.Taffybar.Systray
import System.Taffybar.Battery
import System.Taffybar.Pager
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.MPRIS2
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.Widgets.VerticalBar

import System.Information.Memory
import System.Information.CPU

import Graphics.UI.Gtk

main :: IO ()
main = do
  let bg = (0.11372, 0.11372, 0.11372)

  let memCallback = do
        mi <- parseMeminfo
        return [memoryUsedRatio mi]

      cpuCallback = do
        (_, systemLoad, totalLoad) <- cpuLoad
        return [totalLoad, systemLoad]

      label str color = do
        l <- labelNew Nothing
        labelSetMarkup l (colorize color "" str)
        widgetShowAll l
        return $ toWidget l

  let memCfg = defaultGraphConfig
        { graphDataColors = [(1, 0, 0, 1)]
        , graphLabel = Just $ colorize "#444" "" "  MEM "
        , graphBackgroundColor = bg
        , graphBorderColor = bg
        , graphPadding = 0
       }
      cpuCfg = defaultGraphConfig
        { graphDataColors = [ (0, 1, 0, 1)
                            , (1, 0, 1, 0.5)
                            ]
        , graphLabel = Just $ colorize "#444" "" "  CPU "
        , graphBackgroundColor = bg
        , graphBorderColor = bg
        , graphPadding = 0
        }
      logCfg = defaultPagerConfig
        { activeWorkspace = colorize "yellow" "" . escape
        , emptyWorkspace = colorize "#444" "" . escape
        , hiddenWorkspace = colorize "#888" "" . escape
        , visibleWorkspace = escape
        , widgetSep = colorize "#444" "" "   |   "
       }
      batCfg = defaultBatteryConfig
       { barBorderColor = bg
       , barBackgroundColor = const bg
       , barPadding = 2
       }

  let clock = textClockNew Nothing (colorize "#D64937" "" "%a %b %_d %H:%M") 1
      logger = taffyPagerNew logCfg
      mpris = mpris2New
      bat = batteryBarNew batCfg 10
      bat' = label "  BAT " "#444"
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      tray = systrayNew
      sep = label "   |   " "#444"
      sep' = label "  " ""
      sep'' = label "   " ""

  defaultTaffybar defaultTaffybarConfig
    { barHeight = 23
    , widgetSpacing = 2
    , startWidgets = [ sep', logger ]
    , endWidgets = [ sep'', clock, sep, tray, sep, bat, bat', mem, cpu,
                     sep'', mpris, sep'' ]
    }
