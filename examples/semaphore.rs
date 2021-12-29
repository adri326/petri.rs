#[allow(unused_imports)]
use petri_network::{PetriNetwork, PetriBuilder};
use petri_network::builder::structures::{Semaphore, Mutex, Group};

fn main() -> Result<(), std::io::Error> {
    let mut builder = PetriBuilder::new();
    let coiffeur = builder.node_with_label(1, "coiffeur");
    builder.add_group(coiffeur, "Coiffeur");

    let start = builder.node_with_label(3, "start");
    builder.transition(vec![start], vec![start]); // Clients may wait as much as they want

    let counter_mutex = Mutex::new(&mut builder, 1, "Counter");
    let wake_coiffeur = Semaphore::new(&mut builder, 0);
    let _counter = builder.node_with_label(0, "counter");
    let waiting_line = Semaphore::new(&mut builder, 0);
    let coiffeur_group = Group::new("Coiffeur");

    // Process "Coiffeur":
    builder.begin_branch(coiffeur)
        .with_mod(coiffeur_group.clone())
        .step_with_mod(wake_coiffeur.p())
        .step()
        .step()
        .step()
        .goto("coiffeur");

    // Process "Client"
    builder.begin_branch(start)
        .step_with_mod(counter_mutex.p())
        .with_mod(counter_mutex.section())
        .step()
        .step()
        .with_clone(|branch| {
            branch
                .step_with_condition(["counter"]) // if counter > 0
                .step_with_goto("counter") // counter += 1
                .without_mod()
                .step_with_mod(counter_mutex.v())
                .step_with_mod(waiting_line.p())
                .label("client_1");
        })
        .step_with_goto("counter") // counter += 1
        .without_mod()
        .step_with_mod(counter_mutex.v())
        .join_any(["client_1"])
        .step_with_mod(wake_coiffeur.v())
        .with_mod(coiffeur_group.clone())
        .step()
        .step()
        .step()
        .without_mod()
        .step_with_mod(counter_mutex.p())
        .with_mod(counter_mutex.section())
        .join(["counter"])
        .with_clone(|branch| {
            branch
                .step_with_condition(["counter"])
                .step_with_mod(waiting_line.v())
                .label("client_2");
        })
        .join_any(["client_2"])
        .without_mod()
        .step_with_mod(counter_mutex.v())
        .label("end");
    let end = builder.get_label("end").unwrap().as_node().unwrap();

    let network = builder.build();
    #[cfg(export_dot)]
    {
        let mut file = std::fs::File::create("target/exported.dot")?;
        network.export_dot(&mut file);
        println!("Successfully wrote network into `target/exported.dot`");
    }

    let graph = network.generate_graph();
    graph.assert_always_reaches(|state| state[end] == 3);
    graph.assert_forall(|state| state[wake_coiffeur.index()] <= 1);

    Ok(())
}
