use petri_network::{PetriNetwork, PetriBuilder};
use petri_network::builder::structures::Semaphore;
use std::io::Write;

fn main() -> Result<(), std::io::Error> {
    let mut builder = PetriBuilder::new();
    let start = builder.node_with_label(1, "start");
    let semaphore = Semaphore::new(&mut builder, 1);
    builder.begin_branch(start)
        .step()
        .step_with_branch(|branch| {
            branch
                .step_with_mod(semaphore.p())
                .step()
                .step_with_mod(semaphore.v())
                .label("p1");
        })
        .step_with_mod(semaphore.p())
        .step_with_mod(semaphore.v())
        .join(["p1"])
        .label("end");
    let end = builder.get_label("end").unwrap().as_node().unwrap();

    let network = builder.build();
    let mut file = std::fs::File::create("target/exported.dot")?;
    network.export_dot(&mut file);
    println!("Successfully wrote network into `target/exported.dot`");

    Ok(())
}
