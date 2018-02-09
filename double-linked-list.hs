{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude

import Data.List.NonEmpty

main :: IO ()
main = undefined

newtype DoubleList a = DoubleList (MVar (Maybe (Node a, Node a)))

data Node a = Node
  { val :: a
  , prevNodeMVar :: MVar (Maybe (Node a))
  , nextNodeMVar :: MVar (Maybe (Node a))
  }

emptyDoubleList :: IO (DoubleList a)
emptyDoubleList = fmap DoubleList (newMVar Nothing)

singletonNode :: a -> IO (Node a)
singletonNode a = do
  prev <- newMVar Nothing
  next <- newMVar Nothing
  pure $ Node a prev next

singletonNodes :: a -> IO (Node a, Node a)
singletonNodes a = do
  prev <- newMVar Nothing
  next <- newMVar Nothing
  let node = Node a prev next
  pure (node, node)

singletonDoubleList :: a -> IO (DoubleList a)
singletonDoubleList a = do
  doubleList <- emptyDoubleList
  appendVal a doubleList
  return doubleList

nodesToDoubleList :: (Node a, Node a) -> IO (DoubleList a)
nodesToDoubleList nodes = fmap DoubleList $ newMVar (Just nodes)

linkNodes :: Node a -> Node a -> IO ()
linkNodes
    nodeA@Node{nextNodeMVar=nodeANextNodeMVar}
    nodeB@Node{prevNodeMVar=nodeBPrevNodeMVar} = do
  swapMVar nodeANextNodeMVar (Just nodeB)
  swapMVar nodeBPrevNodeMVar (Just nodeA)
  pure ()

appendVal :: a -> DoubleList a -> IO ()
appendVal a (DoubleList mvar) = do
  maybeNodes <- takeMVar mvar
  case maybeNodes of
    Nothing -> do
      nodes <- singletonNodes a
      putMVar mvar (Just nodes)
    Just (startingNode, endingNode@Node{nextNodeMVar=endingNodeNextNodeMVar}) -> do
      newEndingNode <- singletonNode a
      linkNodes endingNode newEndingNode
      putMVar mvar $ Just (startingNode, newEndingNode)

swapNodePointers :: Node a -> IO ()
swapNodePointers Node{prevNodeMVar, nextNodeMVar} = do
  maybePrevNode <- takeMVar prevNodeMVar
  maybeNextNode <- takeMVar nextNodeMVar
  case maybeNextNode of
    Just nextNode -> swapNodePointers nextNode
    Nothing -> pure ()
  putMVar prevNodeMVar maybeNextNode
  putMVar nextNodeMVar maybePrevNode

reverseDoubleList :: DoubleList a -> IO ()
reverseDoubleList (DoubleList mvar) = do
  maybeNodes <- takeMVar mvar
  case maybeNodes of
    Nothing -> putMVar mvar Nothing
    Just (startNode, endNode) -> do
      swapNodePointers startNode
      putMVar mvar $ Just (endNode, startNode)

fmapNode :: (a -> b) -> Node a -> IO (Node b, Node b)
fmapNode f Node{val=a, prevNodeMVar, nextNodeMVar} = do
  let b = f a
  maybeNextNode <- readMVar nextNodeMVar
  case maybeNextNode of
    Nothing -> singletonNodes b
    Just nextNode -> do
      (newNextNode@Node{prevNodeMVar}, lastNode) <- fmapNode f nextNode
      startingEmptyNode <- newMVar Nothing
      newNextNodeMVar <- newMVar (Just newNextNode)
      let newNode = Node b startingEmptyNode newNextNodeMVar
      swapMVar prevNodeMVar (Just newNode)
      pure (newNode, lastNode)

fmapDoubleList :: (a -> b) -> DoubleList a -> IO (DoubleList b)
fmapDoubleList f (DoubleList mvar) = do
  maybeNodes <- readMVar mvar
  case maybeNodes of
    Nothing -> emptyDoubleList
    Just (startingNode, _) -> fmapNode f startingNode >>= nodesToDoubleList

nodesToList :: Node a -> IO [a]
nodesToList Node{val, nextNodeMVar} = do
  maybeNextNode <- readMVar nextNodeMVar
  case maybeNextNode of
    Nothing -> pure [val]
    Just nextNode -> do
      nextNodesList <- nodesToList nextNode
      pure $ val : nextNodesList

doubleListToList :: DoubleList a -> IO [a]
doubleListToList (DoubleList mvar) = do
  maybeNodes <- readMVar mvar
  case maybeNodes of
    Nothing -> pure []
    Just (startingNode, _) -> nodesToList startingNode

nodesToListRev :: Node a -> IO [a]
nodesToListRev Node{val, prevNodeMVar} = do
  maybePrevNode <- readMVar prevNodeMVar
  case maybePrevNode of
    Nothing -> pure [val]
    Just prevNode -> do
      prevNodesList <- nodesToListRev prevNode
      pure $ val : prevNodesList

doubleListToListRev :: DoubleList a -> IO [a]
doubleListToListRev (DoubleList mvar) = do
  maybeNodes <- readMVar mvar
  case maybeNodes of
    Nothing -> pure []
    Just (_, endingNode) -> nodesToListRev endingNode

listToNodes :: NonEmpty a -> IO (Node a, Node a)
listToNodes (a :| []) = singletonNodes a
listToNodes (a :| (tailA : tailAs)) = do
  (startNode, endNode) <- listToNodes (tailA :| tailAs)
  newStartNode <- singletonNode a
  linkNodes newStartNode startNode
  pure (newStartNode, endNode)

listToDoubleList :: [a] -> IO (DoubleList a)
listToDoubleList [] = emptyDoubleList
listToDoubleList (a:as) = listToNodes (a :| as) >>= nodesToDoubleList
