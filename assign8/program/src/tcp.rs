extern crate rand;

use session::*;
use std::collections::{HashSet, HashMap};

pub struct Syn;
pub struct SynAck;
pub struct Ack;
pub struct Fin;

pub type TCPHandshake<TCPRecv> =
  Recv<Syn, Send<SynAck, Recv<Ack, TCPRecv>>>;

pub type TCPRecv<TCPClose> =
  Rec<Recv<Vec<Packet>,
           Send<Vec<usize>,
                Offer<Var<Z>, TCPClose>>
                >>;

pub type TCPClose =
  Send<Ack, Send<Fin, Recv<Ack, Close>>>;

pub type TCPServer =
  TCPHandshake<TCPRecv<TCPClose>>;

pub type TCPClient = <TCPServer as HasDual>::Dual;


pub fn tcp_server(c: Chan<(), TCPServer>) -> Vec<Buffer> {
  // Handshake
  let (c, _syn) = c.recv();
  let c = c.send(SynAck);
  let (c, _ack) = c.recv();

  // Data Transfer
  let mut c = c.rec_push();
  let mut seq_nums: Vec<usize> = Vec::new();
  let mut packets: Vec<Packet> = Vec::new();

  loop {
    c = {
      let (c, recv_packets) = c.recv();
      let mut recv_seq_nums: Vec<usize> = Vec::new();

      recv_packets.iter().for_each(|p| {
        recv_seq_nums.push(p.seqno.clone());
        if !seq_nums.contains(&p.seqno) {
          seq_nums.push(p.seqno.clone());
          packets.push(p.clone());
        }});
      let c = c.send(recv_seq_nums);

      // Let client decide whether to move to TCPClose
      match c.offer() {
        Branch::Right(c) => {
          // TCPClose
          let (c, _ack) = c.send(Ack).send(Fin).recv();
          c.close();

          // Return buffers sorted by seqno
          packets.sort_by(|a, b| b.seqno.cmp(&a.seqno));
          packets.reverse();
          let mut buffers: Vec<Buffer> = Vec::new();
          packets.iter().for_each(|p| buffers.push(p.buf.clone()));
          return buffers;
        }
        Branch::Left(c) => { c.rec_pop() } // Restart Data Transfer
      }
    }
  }
}

pub fn tcp_client(c: Chan<(), TCPClient>, bufs: Vec<Buffer>) {
  // Handshake
  let c = c.send(Syn);
  let (c, _synack) = c.recv();
  let c = c.send(Ack);

  // Datatransfer
  let mut c = c.rec_push();
  let mut seq_nums = HashSet::new();

  let mut packets_to_send: Vec<Packet> = Vec::new();
  for (i, b) in bufs.iter().enumerate() {
    packets_to_send.push(Packet {buf: b.clone(), seqno: i});
  }

  loop {
    c = {
      // Send packets that have not been received by server
      let c = c.send(packets_to_send.clone());
      let (c, recv_seq_nums) = c.recv();

      // Keep track of received packets in seq_nums
      recv_seq_nums.iter().for_each(|n| {seq_nums.insert(n.clone());});

      // Remove the received packets from the packets vector
      let mut to_delete = Vec::new();
      for (i, p) in packets_to_send.iter().enumerate() {
        if recv_seq_nums.contains(&p.seqno) { to_delete.push(i); }
      }
      to_delete.reverse();
      for i in to_delete {
        packets_to_send.remove(i);
      }

      // All of the packets have been received
      if seq_nums.len() == bufs.len() {
        // Move on to TCPClose
        let (c, _ack) = c.right().recv();
        let (c, _fin) = c.recv();
        c.send(Ack).close();
        return;
      } else {
        c.left().rec_pop()
      }
    }
  }


}

#[cfg(test)]
mod test {
  use session::*;
  use session::NOISY;
  use std::sync::atomic::Ordering;
  use rand;
  use rand::Rng;
  use tcp::*;
  use std::thread;

  fn gen_bufs() -> Vec<Buffer> {
    let mut bufs: Vec<Buffer> = Vec::new();
    let mut rng = rand::thread_rng();
    for _ in 0usize..20 {
      let buf: Buffer = vec![0; rng.gen_range(1, 10)];
      let buf: Buffer = buf.into_iter().map(|_| rng.gen()).collect();
      bufs.push(buf);
    }
    bufs
  }

  #[test]
  fn test_basic() {
    let bufs = gen_bufs();
    let bufs_copy = bufs.clone();
    let (s, c): ((Chan<(), TCPServer>), (Chan<(), TCPClient>)) = Chan::new();
    let thread = thread::spawn(move || { tcp_client(c, bufs); });

    let recvd = tcp_server(s);
    thread.join().unwrap();

    assert_eq!(recvd, bufs_copy);
  }

  #[test]
  fn test_lossy() {
    let bufs = gen_bufs();
    let bufs_copy = bufs.clone();

    NOISY.with(|noisy| {
      noisy.store(true, Ordering::SeqCst);
    });

    let (s, c): ((Chan<(), TCPServer>), (Chan<(), TCPClient>)) = Chan::new();
    let thread = thread::spawn(move || { tcp_client(c, bufs); });

    let recvd = tcp_server(s);
    thread.join().unwrap();

    assert_eq!(recvd, bufs_copy);
  }
}
