** Com fer reducions de tiles mes eficients (iwomp09)

    #pragma hlt blocking + #pragma omp for reduction(...)
  * #pragma hlt task-aggregate?
  * potser podriem fer que aixo de la agregacio fos una generacio que pogues escollir el runtime

    if (many_tasks) use_aggregated_tasks()

    else use_individual_tasks()
 
