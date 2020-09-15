var { Elm } = require('./elm');
var assert = require('assert-diff');
var test = require('baretest')('elm-climate');
var {
    CExperienceValues,
    CModel,
    CSimulationValues,
} = require("simclimat-lib");

var elmTestWrapper = Elm.TestWrapper.init();

test('gives the same results as simclimat-lib', async () => {
    const config = {
        fixed_eau: false,
        fixed_concentration: false,
        debranche_biologie: false,
        fixed_ocean: false,
        debranche_ocean: false,
        fixed_albedo: true,
        rapport_H2O_value: 100,
        puit_bio_value: 35,
        puit_oce_value: 20,
        albedo_value: 33,
        coo_concentr_value: 280,
        puissance_soleil_value: 100,
        distance_ts_value: 100,
        obliquite_value: 23.5,
        excentricite_value: 0.0167,
        precession_value: 102.7,
        alteration_value: 100,
        emit_anthro_coo_value: 0,
        volcan_value: 0.083,
        stockage_biologique_value: 0,
    };

    assert.deepEqual(
        await runElmClimateAsync(config),
        runSimClimat(config)
    );
});

var runElmClimateAsync = config =>
    new Promise((resolve, reject) => {
        let unsubscribe;
        try {
            unsubscribe = elmTestWrapper.ports.output.subscribe(resolve);
            elmTestWrapper.ports.input.send(config);
        } catch (err) {
            reject(err);
        } finally {
            if (unsubscribe != null) {
                unsubscribe();
            }
        }
    });

var runSimClimat = config => {
    const years /*: numbers */ = 10000;
    const sv /*: CSimulationValues */ = new CSimulationValues();
    const ev /*: CExperienceValues */ = new CExperienceValues(years);
    const m /*: CModel */ = new CModel();

    // Here you should call either create_actual_state() or create_1750_state() of CSimulationValues object to init properties and datas arrays.
    sv.create_1750_state();

    // Here you can change CSimulationValues properties before calling modelExecute
    sv.fixed_albedo = true;
    sv.albedo_value = 33;

    // Call modelExecute method to calculate climates datas
    const output = m.modelExecute(sv, ev);

    // FIXME
    delete output['simulation_name'];
    delete output['ID_MIN'];
    delete output['ID_MAX'];
    delete output['TEMPS_ELEM'];
    delete output['INTERN_ECHEANCE'];
    delete output['modelPhysicsConstants'];
    delete output['modelVarsConstants'];
    delete output['emissions_coo_data'];
    delete output['concentrations_coo_data'];
    delete output['temperature_data'];
    delete output['niveau_calottes_data'];
    delete output['niveau_mer_data'];
    delete output['albedo_data'];
    delete output['annee_debut'];
    delete output['annee_fin'];

    return output;
};

test.run();
