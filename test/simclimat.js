var { Elm } = require('./elm');
var assert = require('assert-diff');
var test = require('baretest')('elm-climate');
var _ = require('lodash');
var {
    CExperienceValues,
    CModel,
    CSimulationValues,
} = require("simclimat-lib");

assert.options.strict = true

var elmTestWrapper = Elm.TestWrapper.init();

test('starting from pre-industrial state', async () => {
    const sv = new CSimulationValues();
    sv.create_1750_state();
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

test('starting from now', async () => {
    const sv = new CSimulationValues();
    sv.create_actual_state();
    sv.annee_debut = 2007.0;
    // logSimulationValues(sv);
    await assertSameResultsAsync(sv);
});

var assertSameResultsAsync = async sv => {
    assert.deepEqual(
        runSimClimat(sv),
        await runElmClimateAsync(sv)
    );
};

var runElmClimateAsync = config =>
    new Promise((resolve, reject) => {
        let unsubscribe;
        try {
            unsubscribe = elmTestWrapper.ports.output.subscribe(sv => {
                resolve(normalizeSimulationValues(sv));
            });
            elmTestWrapper.ports.input.send(config);
        } catch (err) {
            reject(err);
        } finally {
            if (unsubscribe != null) {
                unsubscribe();
            }
        }
    });

var runSimClimat = sv => {
    const years /*: numbers */ = 10000;
    // const sv /*: CSimulationValues */ = new CSimulationValues();
    const ev /*: CExperienceValues */ = new CExperienceValues(years);
    const m /*: CModel */ = new CModel();

    // Here you should call either create_actual_state() or create_1750_state() of CSimulationValues object to init properties and datas arrays.
    // sv.create_1750_state();

    // Here you can change CSimulationValues properties before calling modelExecute
    // sv.fixed_albedo = true;
    // sv.albedo_value = 33;

    // Call modelExecute method to calculate climates datas
    return normalizeSimulationValues(
        m.modelExecute(sv, ev)
    );
};

var logSimulationValues = sv => {
    console.log(JSON.stringify(normalizeSimulationValues(sv), null, 2));
}

var normalizeSimulationValues = sv => {
    sv = _.cloneDeep(sv);
    // FIXME
    delete sv['simulation_name'];
    delete sv['ID_MIN'];
    delete sv['ID_MAX'];
    delete sv['TEMPS_ELEM'];
    delete sv['INTERN_ECHEANCE'];
    delete sv['modelPhysicsConstants'];
    delete sv['modelVarsConstants'];
    delete sv['emissions_coo_data'];
    delete sv['concentrations_coo_data'];
    // delete sv['temperature_data'];
    if (sv['temperature_data']) {
        delete sv['temperature_data']['N'];
        delete sv['temperature_data']['datas'];
    }
    delete sv['niveau_calottes_data'];
    delete sv['niveau_mer_data'];
    delete sv['albedo_data'];
    // delete sv['annee_debut'];
    delete sv['annee_fin'];

    return sv;
}

test.run();