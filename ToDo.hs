import Graphics.UI.Gtk
import Data.IORef  
  
main :: IO ()
main = do

  initGUI

  window  <- windowNew
  set window [windowTitle := "To do list",
              windowDefaultWidth := 700,
              windowDefaultHeight := 700]

  table   <- tableNew 10 10 True  --Grid to manage the widgets places
  containerAdd window table
  let attach x y w h item = tableAttachDefaults table item x y w h

  addNewTask_lbl <- labelNew (Just "Add New Task")
  tableAttachDefaults table addNewTask_lbl 0 2 0 1

  newTask_txtF <- entryNew
  tableAttachDefaults table newTask_txtF 2 5 0 1

  add_btn <- buttonNewWithLabel "Add"
  tableAttachDefaults table add_btn 0 2 1 2

  clear_btn <- buttonNewWithLabel "Clear All"
  tableAttachDefaults table clear_btn 3 5 1 2

  addNewTask_lbl <- labelNew (Just "My Task List")
  tableAttachDefaults table addNewTask_lbl 0 5 2 3

  done_lbl <- labelNew (Just "Done")
  tableAttachDefaults table done_lbl 5 10 2 3

  taskRow1_c <- makeCounter 3 --make global variable to track the row number that is empty
  taskRow2_c <- makeCounter 4 -- same

  doneTaskRow1_c <- makeCounter 3 -- same for done list
  doneTaskRow2_c <- makeCounter 4 -- same

  onDestroy window mainQuit
  
  add_btn `onClicked` do 
    task <- entryGetText newTask_txtF 
    putStrLn task
    place1 <- showCounter taskRow1_c -- row 1 for new task
    place2 <- showCounter taskRow2_c  -- row 2 for new task
    place3 <- showCounter doneTaskRow1_c -- row 1 for done task
    place4 <- showCounter doneTaskRow2_c -- row 2 for done task
    (mkBtn task table place3 place4 clear_btn) >>= attach 0 2 place1 place2
    incCounter 1 taskRow1_c
    incCounter 1 taskRow2_c
    incCounter 1 doneTaskRow1_c
    incCounter 1 doneTaskRow2_c
    entrySetText newTask_txtF ""
    containerAdd window table 
    widgetShowAll table
    widgetShowAll window

  clear_btn `onClicked` do -- clear the counters to start from the beginning again
    counter1 <- showCounter taskRow1_c
    counter2 <- showCounter taskRow2_c
    counter3 <- showCounter doneTaskRow1_c
    counter4 <- showCounter doneTaskRow2_c
    incCounter (-counter1+3) taskRow1_c
    incCounter (-counter2+4) taskRow2_c
    incCounter (-counter3+3) doneTaskRow1_c
    incCounter (-counter4+4) doneTaskRow2_c

  widgetShowAll window

  mainGUI

mkBtn label tab r1done r2done clrbtn = do
  btn <- checkButtonNew
  doneTask_lbl <- labelNew (Just label)
  set btn [ buttonLabel := label ]
  btn `onToggled` do 
    widgetDestroy btn  -- Remove task from task list
    tableAttachDefaults tab doneTask_lbl 5 10 r1done r2done  -- Add task to done list 
    widgetShowAll tab
  clrbtn `onClicked` do
    widgetDestroy btn
    widgetDestroy doneTask_lbl
  return btn

data Counter = Counter { x :: IORef Int } -- io ref for tracking the state of the app 

makeCounter :: Int -> IO Counter        
makeCounter i = do 
  iref <- newIORef i   
  return (Counter iref)

incCounter :: Int -> Counter -> IO ()            
incCounter i (Counter c) = do 
  modifyIORef c (+ i)

showCounter :: Counter -> IO Int               
showCounter (Counter c) = do
  c' <- readIORef c
  return(c')     

