//// The `client_registry` module holds a list of connected clients (browsers)
//// and allows to trigger those clients by sending them a `Reload` message.

import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/set.{type Set}
import olive/logging

pub type ClientMessage {
  Reload
}

pub opaque type ClientRegistry {
  ClientRegistry(Subject(Message))
}

type Message {
  ClientConnected(Subject(ClientMessage))
  ClientDisconnected(Subject(ClientMessage))
  TriggerClients
}

pub fn start(logger: logging.Logger) {
  let assert Ok(actor) =
    actor.new(set.new())
    |> actor.on_message(
      fn(state: Set(Subject(ClientMessage)), message: Message) {
        case message {
          ClientConnected(client) -> {
            logging.debug(logger, "Client connected")
            actor.continue(set.insert(state, client))
          }
          ClientDisconnected(client) -> {
            logging.debug(logger, "Client disconnected")
            actor.continue(set.delete(state, client))
          }
          TriggerClients -> {
            case set.is_empty(state) {
              True -> actor.continue(state)
              False -> {
                logging.debug(logger, "Triggering clients to reload")
                set.each(state, fn(client) { process.send(client, Reload) })
                actor.continue(state)
              }
            }
          }
        }
      },
    )
    |> actor.start
  let subject = actor.data
  ClientRegistry(subject)
}

pub fn add(registry: ClientRegistry, client: Subject(ClientMessage)) {
  let ClientRegistry(subject) = registry
  actor.send(subject, ClientConnected(client))
}

pub fn remove(registry: ClientRegistry, client: Subject(ClientMessage)) {
  let ClientRegistry(subject) = registry
  actor.send(subject, ClientDisconnected(client))
}

pub fn trigger(registry: ClientRegistry) {
  let ClientRegistry(subject) = registry
  actor.send(subject, TriggerClients)
}
