program funcdefShadowBuiltinTest

int *pvGet; /* error: 'pvGet' already (implicitly) declared */

ss simple {
    state simple {
        when () {
            int *pvGet;
        } exit
    }
}
