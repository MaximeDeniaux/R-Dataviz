

# Interactive Word Search (Mots Meles) - HTML Generator

# This R script:
#   1. Defines FR/EN dictionaries (~1300 FR words, ~1200 EN words)
#   2. Defines mystery words (~44 FR, ~40 EN)
#   3. Implements the grid generation algorithm in R
#      (word placement, direction management, mystery word)
#   4. Pre-generates a grid for the default configuration
#   5. Converts all data to JSON via jsonlite
#   6. Injects JSON into the HTML template and writes the final file

# The JS embedded in the template mirrors the same algorithm to allow
# grid regeneration on the browser side without page reload.


library(jsonlite)
library(here)


# Dictionaries  ----

# French words (no accents, uppercase, 3-10 letters) 

mots_fr <- c(
  "ABORD","ABSENT","ACCENT","ACCORD","ACTION","ADIEU","ADRESSE","ADULTE",
  "AFFAIRE","AGENCE","AGENT","AGIR","AIDER","AILE","AIMER","AINSI",
  "AJOUTER","ALERTE","ALLUMER","AMENER","AMOUR","ANCIEN","ANGLE","ANIMAL",
  "ANNEE","ANNONCER","APPEL","APPELER","APPORTER","APPUYER","ARBRE","ARGENT",
  "ARME","ARMEE","ARRACHER","ARRETER","ARRIVER","ARTICLE","ASSEZ","ASSIS",
  "ASSURER","ATELIER","ATTACHER","ATTAQUER","ATTENDRE","ATTIRER","AUCUN",
  "AUDACE","AUGMENTER","AUTEUR","AVANCER","AVENIR","AVENTURE","AVERTIR",
  "AVION","AVOIR","BAGAGE","BAISSER","BALCON","BALLE","BALLON","BANDE",
  "BANQUE","BARBE","BARQUE","BARRE","BATEAU","BATTRE","BESOIN","BILLET",
  "BLANC","BLESSE","BLEU","BOIRE","BOISERIE","BONHEUR","BORD","BOUCHE",
  "BOUGER","BOULANGER","BOURSE","BRANCHE","BRAVE","BRISER","BRONZE","BRUIT",
  "BRUME","BUREAU","CABANE","CACHER","CADEAU","CADRE","CAHIER","CALME",
  "CAMPAGNE","CANAL","CAPABLE","CAPITALE","CAPTER","CARNET","CARRE","CARTE",
  "CASQUE","CASSER","CAUSE","CELLULE","CENTRE","CERCLE","CHAINE","CHAISE",
  "CHAMBRE","CHANCE","CHANGER","CHANTER","CHARGE","CHARGER","CHASSE","CHAUD",
  "CHEMIN","CHEVAL","CHIEN","CHIFFRE","CHOIX","CHOSE","CIBLE","CIEL",
  "CIRQUE","CLAIR","CLASSER","CLIMAT","COEUR","COFFRE","COGNER","COLLER",
  "COLLINE","COMBAT","COMMANDE","COMPLET","COMPTER","CONDUIRE","CONFIER",
  "CONSEIL","CONTENIR","CONTENT","CONTRAT","CONTRE","COPIE","CORDE","CORPS",
  "CORRECT","COUCHER","COULER","COULEUR","COULOIR","COUPER","COUPLE","COURAGE",
  "COURANT","COURIR","COURT","COUVRIR","CRACHER","CRAINDRE","CREER","CREUSER",
  "CRIER","CRIME","CRISE","CROIRE","CROISER","CRUEL","CUISINE","CULTE",
  "CURIEUX","CYCLE","DANGER","DEBOUT","DECIDER","DEDANS","DEFAUT","DEFENDRE",
  "DEHORS","DELAI","DEMANDER","DEMARRER","DEMEURER","DEPART","DEPUIS","DERIVE",
  "DERNIER","DERRIERE","DESIR","DESSOUS","DESSUS","DETRUIRE","DEVANT","DEVENIR",
  "DEVOIR","DIABLE","DIGNE","DIRIGER","DISCOURS","DISCRET","DISTANCE","DIVERS",
  "DOIGT","DOMAINE","DONNER","DORMIR","DOUBLE","DOUCHE","DOUCEUR","DOULEUR",
  "DOUTER","DRESSER","DROITE","DROLE","DUREE","ECHEC","ECHELLE","ECLAIR",
  "ECLATER","ECOLE","ECOUTER","ECRIRE","EFFORT","EGLISE","ELARGIR","ELEVER",
  "ELOIGNER","EMPLOI","EMPORTER","ENCADRER","ENDROIT","ENERGIE","ENFANT",
  "ENFERMER","ENLEVER","ENNEMI","ENNUYER","ENORME","ENSEMBLE","ENTENDRE",
  "ENTIER","ENTOUR","ENTRER","ENVOYER","EPAULE","EPOQUE","EPREUVE","EQUIPE",
  "ERREUR","ESPACE","ESPOIR","ESPRIT","ESSAYER","ETAGE","ETALER","ETOILE",
  "ETRANGE","ETROIT","ETUDE","EVALUER","EXACT","EXEMPLE","EXERCER","EXIGER",
  "EXPERT","EXPOSER","FACADE","FACTEUR","FAIBLE","FAILLE","FAIRE",
  "FALAISE","FAMILLE","FATIGUE","FAUTE","FAVEUR","FERMER","FEUILLE","FIABLE",
  "FICHER","FIGURER","FILLE","FILTRE","FINIR","FLAMME","FLECHE","FLEUR",
  "FLEUVE","FLOTTE","FLUIDE","FOIRE","FONCER","FONDRE","FORCE","FORGER",
  "FORME","FORMULE","FORTE","FORTUNE","FOUILLE","FOULE","FOURCHE","FRAGILE",
  "FRAIS","FRAPPER","FROID","FRONT","FUIR","FUMER","FURIEUX","GAGNER",
  "GALERIE","GARDER","GAUCHE","GENERAL","GENOU","GLACE","GLISSER","GLOBAL",
  "GORGE","GOUTTE","GRAIN","GRANDE","GRAVER","GRIMPER","GRISE","GROSSE",
  "GROUPE","GUERRE","GUIDER","HABITER","HAINE","HASARD","HAUSSE","HERBE",
  "HEURE","HIVER","HOMME","HUMAIN","HUMIDE","HUMOUR","IDEE","IMAGE",
  "IMPOSER","INCONNU","INGRAT","INJUSTE","INQUIET","INSCRIRE","INSTALLER",
  "INSTANT","INTERET","INUTILE","INVITER","ISOLER","JAMAIS","JARDIN","JAUNE",
  "JETER","JOINDRE","JOUEUR","JOURNAL","JUGER","JURER","JUSTICE","LACHER",
  "LAINE","LAISSER","LANCER","LANGUE","LARGE","LAVER","LECON","LECTURE",
  "LEGER","LENTE","LETTRE","LEVER","LIBRE","LIER","LIGNE","LIMITE","LINGE",
  "LISSE","LISTE","LIVRER","LOGIQUE","LOISIR","LONGER","LOURD","LUEUR",
  "LUMIERE","LUNETTE","LUTTER","MACHINE","MAITRE","MALADIE","MANGER","MANQUER",
  "MARCHE","MARCHER","MARGE","MARINE","MARQUE","MASSE","MATIERE","MATIN",
  "MEILLEUR","MELANGE","MEMBRE","MEMOIRE","MENACER","MESSAGE","METIER","METTRE",
  "MEUBLE","MIEUX","MILIEU","MINCE","MINE","MINUTE","MIROIR","MISSION",
  "MOBILE","MODELE","MOINDRE","MOITIE","MOMENT","MONDE","MONTAGNE","MONTER",
  "MONTRER","MORALE","MORCEAU","MORDRE","MOURIR","MURER","MUSCLE",
  "MUSIQUE","NAGER","NAITRE","NATURE","NAVIRE","NEIGER","NERVEUX","NEUTRE",
  "NIVEAU","NOBLE","NOMBRE","NOMMER","NORMAL","NOTER","NOURRIR","NOUVEAU",
  "NUAGE","NUIRE","NUMERO","OBJET","OBLIGER","OBTENIR","OCCUPER","OFFRIR",
  "OISEAU","OMBRE","ONCLE","ONGLE","OPERER","OPPOSER","ORANGE","ORDRE",
  "OREILLE","ORGANE","OSER","OUBLI","OUVERT","OUVRIR","PANIER","PAPIER",
  "PARDON","PARENT","PARFAIT","PARLER","PAROLE","PARTIE","PARVENIR","PASSER",
  "PATRON","PAUVRE","PAYER","PEINDRE","PENCHER","PERDRE","PERIODE","PERMIS",
  "PESER","PHOTO","PHRASE","PIECE","PIERRE","PILOTE","PIQUER","PISTE",
  "PLACE","PLACER","PLAGE","PLAINDRE","PLAIRE","PLANCHE","PLANTER","PLEIN",
  "PLEURER","PLIER","PLONGER","PLUIE","POCHE","POINTE","POISON","POLICE",
  "POMME","PONDRE","PORTER","POSER","POUMON","POUSSER","POUVOIR","PRATIQUE",
  "PREMIER","PRENDRE","PREPARER","PRESENT","PRESQUE","PRETER","PREUVE","PRIER",
  "PRINCE","PRISON","PRIVER","PROCHE","PRODUIRE","PROFOND","PROJET","PROPRE",
  "PROTEGER","PUBLIC","PUISER","PUNIR","QUALITE","QUITTER","RACINE","RACONTER",
  "RAIDE","RAISON","RAMENER","RANGER","RAPIDE","RAPPEL","RASER","RATER",
  "RAYON","RECEVOIR","RECULER","REDUIRE","REFUSER","REGARD","REGION","RELIER",
  "REMARQUER","REMPLIR","RENDRE","RENONCER","REPARER","REPAS","REPETER",
  "REPLIER","REPONDRE","REPOS","REPRISE","RESOUDRE","RESPECT","RESTER","RETARD",
  "RETENIR","RETIRER","RETOUR","REUNIR","REVEIL","REVELER","REVENIR","RICHESSE",
  "RIDEAU","RIEN","RIGIDE","RISQUER","RIVAGE","RIVIERE","ROCHE","ROMPRE",
  "RONDE","ROUGE","ROULER","ROUTE","RUINER","SABLE","SAISIR","SAISON",
  "SALLE","SALUER","SANCTION","SAUTER","SAUVAGE","SAUVER","SAVOIR","SCENE",
  "SCOLAIRE","SECHER","SECOURS","SECRET","SEIGNEUR","SEMAINE","SEMBLER","SENTIR",
  "SEREIN","SERGENT","SERRE","SERVIR","SEUIL","SIGNAL","SIGNER","SILENCE",
  "SIMPLE","SINGER","SOCIAL","SOLDAT","SOLEIL","SOLIDE","SOMBRE","SOMMET",
  "SONGER","SONNER","SORTIR","SOUFFLER","SOUFFRIR","SOURCE","SOURIRE","SOUTENIR",
  "SUIVANT","SUIVRE","SUPERBE","SURFACE","SURTOUT","TACHER","TAILLE","TAPER",
  "TENDRE","TENIR","TENTER","TERMINER","TERRAIN","TESTER","TIRER","TISSER",
  "TITRE","TOILE","TOMBER","TONNERRE","TOUCHER","TOUJOURS","TOURNER","TOUSSER",
  "TRACER","TRAINER","TRAITER","TRAVAIL","TREMBLER","TRISTE","TROMPER","TROUVER",
  "UNIQUE","URGENT","UTILE","VAGUE","VAINCRE","VALEUR","VALISE","VALOIR",
  "VAPEUR","VEILLE","VENDRE","VENGER","VERSER","VIBRER","VIDER","VILLAGE",
  "VIOLENT","VISAGE","VISER","VISION","VISITE","VIVANT","VIVRE","VOITURE",
  "VOLEUR","VOULOIR","VOYAGE","VRAI","ABRICOT","AIGUILLE","ALLIANCE","AMBIANCE",
  "AMPOULE","ANCHOIS","ANTENNE","ARAIGNEE","ARCHIVE","ARTISAN","ASSIETTE",
  "ATOUT","ATTAQUE","AUTOMNE","AVEUGLE","BALANCE","BANQUIER","BAROQUE","BASSIN",
  "BERGER","BEURRE","BIJOU","BISCUIT","BLESSER","BOCAL","BORGNE","BOUCLIER",
  "BOUGIE","BOULEAU","BOUTIQUE","BRIQUE","BROCHURE","BRODERIE","BROYER",
  "BRULURE","BUISSON","CACTUS","CALIBRE","CAMARADE","CANARD","CAPRICE",
  "CAPTURER","CARESSE","CARNAGE","CAROTTE","CARREAU","CASCADE","CASERNE",
  "CAVERNE","CERISE","CERVEAU","CHAPITRE","CHARBON","CHARIOT","CHEVILLE",
  "CIGALE","CITOYEN","CLAVIER","CLOCHER","COCHON","COLERE","COLONNE",
  "COMPOSER","CONCERT","CONFITURE","CONQUETE","CONVENIR","COPEAUX","COQUILLE",
  "CORNICHE","COSTUME","COUSSIN","CRAVATE","CREATEUR","CROCHET","CUISSON",
  "CULTURE","CYCLONE","DAUPHIN","DEBUTER","DECORER","DEFILER","DESIGNER",
  "DESSINER","DETECTER","DIAMANT","DIFFUSER","DRAPEAU","ECHAPPER","EFFACER",
  "ELEGANT","EMOTION","EMPECHER","EMPLOYER","ENCLUME","ENDURER","ENIGME",
  "ENSEIGNER","ENTAMER","ENVAHIR","EPOUSER","ESCALIER","ESQUIVER","ESTIMER",
  "ETAGERE","ETENDRE","EVENTAIL","EXAUCER","EXERCICE","EXPLORER","FANTOME",
  "FARCEUR","FAUTEUIL","FENETRE","FERMIER","FICELLE","FILMER","FISSURE",
  "FLEURIR","FLOCON","FONDERIE","FOUGERE","FOURNIR","FRICTION","FROMAGE",
  "FRONTAL","FUGITIF","GALOPER","GAMELLE","GARENNE","GAZETTE","GELATINE",
  "GESTION","GLACIER","GOUFFRE","GOURMET","GRENIER","GRILLAGE","GRIMACE",
  "GUICHET","HABITUDE","HARICOT","HEBERGER","HERITAGE","HORIZON","HOSTILE",
  "HUMILIER","HURLER","IGNORER","ILLUSTRE","IMMOBILE","IMPRIMER","INCLINER",
  "INDIQUER","INFIRME","INONDER","INSPIRER","INVENTIF","IVRESSE","JAILLIR",
  "JALOUSIE","JARDINER","JONGLER","JUBILER","JUMELLES","KIOSQUE","LABEUR",
  "LACERER","LAITIER","LAMBINER","LAQUAIS","LAURIER","LEGENDE","LENTEUR",
  "LEVRIER","LIBERER","LICENCE","LINGERIE","LISIERE","LITIERE","LORGNER",
  "LOSANGE","LOUANGE","LUNAIRE","MACHOIRE","MAGASIN","MAILLON","MALAISE",
  "MANIABLE","MANSARDE","MARELLE","MARIAGE","MATELAS","MAUDIRE","MECHANT",
  "MEDECIN","MEDITER","MENSONGE","MIRACLE","MOBILIER","MOLLESSE","MOQUETTE",
  "MORSURE","MOTIVER","MOUILLER","MURMURE","MYSTERE","NAUFRAGE","NECTAR",
  "NERVURE","NOTABLE","NOURRICE","NUISIBLE","OBSTACLE","OBTURER","OFFICIER",
  "OMELETTE","ONDOYER","OPULENT","ORAGEUX","ORIGAMI","ORGUEIL","OUVRAGE",
  "PAISIBLE","PANACHE","PANIQUE","PARADIS","PARCELLE","PARESSER","PARTAGER",
  "PASSAGE","PATIENCE","PELOUSE","PENSEUR","PERDRIX","PINCEAU","PLANCHER",
  "PLAISIR","PLATEAU","PLUMAGE","POIGNARD","POLISSON","PORTRAIT","POTAGER",
  "PRESTIGE","PREVENIR","PRIORITE","PROBLEME","PROFITER","PROMENER","PROPOSER",
  "PROVINCE","PRUDENCE","QUENELLE","QUERELLE","RABOTER","RAFFINER","REBONDIR",
  "RECLAMER","RECOLTER","REGORGER","REMORQUER","RENFORCER","REPOUSSER",
  "RESERVER","RESONNER","RETROUVER","ROBUSTE","ROUILLER","RUISSEAU","SABLIER",
  "SABORDER","SACREMENT","SAGEMENT","SALARIER","SANGLIER","SATELLITE","SCALPEL",
  "SCORPION","SEMBLABLE","SENSIBLE","SERVIETTE","SINISTRE","SOIGNEUR","SOLITUDE",
  "SORCIERE","SOUVENIR","SPECTACLE","SPLENDEUR","SUBMERGER","SUFFISANT",
  "SUPPLICE","SURVENIR","SUSPENDRE","SYMBOLE","TABLETTE","TAMBOUR","TENDRESSE",
  "TERMINUS","TERRIBLE","TIGRESSE","TOILETTE","TOURMENTE","TRAHISON","TRANCHEE",
  "TRIBUNAL","TRICOTER","TRIOMPHER","TUMULTE","UNIFORME","UNIVERSEL","VACARME",
  "VAGABOND","VENGEANCE","VERDURE","VIGNOBLE","VIRTUOSE","VOCATION","VOLATILE",
  "VROMBIR","ABATTRE","ACCEPTER","ACCOMPLIR","ACCORDER","ACCROCHER","ACHETER",
  "ADMETTRE","ADOPTER","ADRESSER","AFFIRMER","AGRANDIR","ALIMENTER","AMELIORER",
  "ANALYSER","ANGOISSE","APPLIQUER","APPRENDRE","ARCHIVER","ASSEMBLER",
  "ASSISTER","ASSOCIER","ATTRIBUER","AVENTURER","BAPTISER","BENEFICE",
  "BIENVENU","BOUILLON","BRANCHER","BRICOLER","BROUILLER","CALCULER",
  "CANDIDAT","CAPITAINE","CAPTIVER","CEINTURE","CENSURER","CHANTIER",
  "CHEMINER","CHERCHER","CHRONIQUE","CIRCULER","CLOISON","COIFFER","COLLOQUE",
  "COMBATTRE","COMMANDER","COMPENSER","COMPLETER","CONCEVOIR","CONCILIER",
  "CONDAMNER","CONFIANCE","CONFIRMER","CONQUERIR","CONSERVER","CONSUMER",
  "CONTESTER","CONTINUER","CONTRASTE","CORRIGER","COURTISER","COUVERCLE",
  "CRITIQUER","CUEILLIR","CULTIVER","DEBATTRE","DECEVOIR","DECLARER",
  "DEGUISER","DELIVRER","DEMOLIR","DEMONTRER","DENONCER","DEPLOYER","DEPOSER",
  "DESTINER","DETACHER","DETOURNER","DEVALUER","DEVANCER","DIALOGUER",
  "DIFFAMER","DIMINUER","DISCERNER","DISPOSER","DISPUTER","DIVULGUER",
  "DOMINER","ECHANGER","ECLAIRER","ELABORER","EMBRASSER","EMIGRER",
  "EMPRUNTER","ENCAISSER","ENCOURAGER","ENGAGER","ENRICHIR","ENTOURER",
  "ENVISAGER","ESCOMPTER","ETABLIR","EVACUER","EXAMINER","EXECUTER","EXISTER",
  "EXPLOITER","EXPRIMER","EXTRAIRE","FABRIQUER","FACILITER","FAVORISER",
  "FINANCER","FISSURER","FORMULER","FRANCHISE","GASPILLER","GOUVERNER",
  "GRADUER","GRIMACER","HABILLER","HARMONISER","HONORER","HORLOGER",
  "ILLUSTRER","IMAGINER","IMMERGER","IMPLANTER","IMPORTER","INAUGURAL",
  "INCENDIER","INFORMER","INITIER","INNOVER","INSPECTER","INSTRUIRE",
  "INVOQUER","IRRIGUER","JUSTIFIER","KIDNAPPER","LABOURER","LICENCIER",
  "LIQUIDER","LOCALISER","MAINTENIR","MAJORER","MANIPULER","MAQUILLER",
  "MASSACRER","MAXIMISER","MEMORISER","MENUISIER","NAVIGUER","NEGLIGENT",
  "NEGOCIER","NETTOYER","OBJECTER","OBSERVER","ORGANISER","ORIENTER",
  "OUTILLER","PARCOURIR","PATIENTER","PATRONNER","PERFORER","PERMETTRE",
  "PERSUADER","PLANIFIER","PRECISER","PRESERVER","PRESENTER","PROCURER",
  "PROLONGER","PRONONCER","PUBLIER","QUALIFIER","RATTRAPER","REALISER",
  "RECHARGER","RECUPERER","REFORMER","REGROUPER","REMANIER","RENCONTRER",
  "REPANDRE","RESISTER","RETABLIR","SIGNALER","SOUMETTRE","STIMULER",
  "SUBSISTER","SUCCEDER","SUPERVISER","SUPPORTER","SURMONTER","TROUBLER",
  "URBANISER","VALORISER","VERIFIER"
)


mots_fr <- unique(mots_fr[nchar(mots_fr) >= 3 & nchar(mots_fr) <= 10])



# English words (3-10 letters) 

mots_en <- c(
  "ABLE","ABOUT","ABOVE","ACCEPT","ACROSS","ACTION","ACTUAL","ADDED",
  "ADMIT","ADOPT","ADULT","ADVICE","AFFORD","AFTER","AGAIN","AGENT",
  "AGREE","AHEAD","ALARM","ALBUM","ALERT","ALIVE","ALLOW","ALONE",
  "ALONG","ALTER","ALWAYS","AMAZE","AMONG","AMOUNT","ANGER","ANGLE",
  "ANGRY","ANIMAL","ANNUAL","ANSWER","ANYONE","APART","APPEAL","APPEAR",
  "APPLE","APPLY","ARGUE","ARISE","ARMOR","ARRAY","ARROW","ASIDE",
  "ASSET","ASSIST","ASSUME","ATTACH","ATTACK","ATTEND","AVOID","AWAKE",
  "AWARE","BADGE","BAKER","BASIC","BASIN","BATCH","BEACH","BEAST",
  "BEGIN","BEING","BENCH","BERRY","BIRTH","BLACK","BLADE","BLAME",
  "BLANK","BLAST","BLAZE","BLEED","BLEND","BLESS","BLIND","BLOCK",
  "BLOOD","BLOWN","BOARD","BONUS","BOOTH","BOUND","BRAIN","BRAND",
  "BRAVE","BREAD","BREAK","BREED","BRICK","BRIEF","BRING","BROAD",
  "BROKE","BROOK","BROWN","BRUSH","BUILD","BUNCH","BURST","BUYER",
  "CABIN","CABLE","CAMEL","CANDY","CARGO","CARRY","CATCH","CAUSE",
  "CHAIN","CHAIR","CHALK","CHARM","CHASE","CHECK","CHEEK","CHEER",
  "CHESS","CHEST","CHIEF","CHILD","CHILL","CHINA","CHUNK","CIVIL",
  "CLAIM","CLASH","CLEAR","CLERK","CLIFF","CLIMB","CLING","CLOCK",
  "CLOSE","CLOUD","COACH","COAST","COLOR","COMET","CORAL","COUNT",
  "COURT","COVER","CRACK","CRAFT","CRASH","CRAZY","CREAM","CRIME",
  "CRISP","CROSS","CROWD","CRUEL","CRUSH","CURVE","CYCLE","DAILY",
  "DANCE","DEATH","DEBUT","DECOR","DELAY","DENSE","DEPTH","DEVIL",
  "DIRTY","DITCH","DONOR","DOUBT","DRAFT","DRAIN","DRAMA","DRAWN",
  "DREAM","DRESS","DRIED","DRIFT","DRILL","DRINK","DRIVE","DRONE",
  "DROPS","DROWN","DRUNK","DYING","EAGER","EARTH","EIGHT","ELDER",
  "ELECT","ELITE","EMPTY","ENEMY","ENJOY","ENTER","EQUAL","ERROR",
  "EVENT","EVERY","EXACT","EXIST","EXTRA","FABLE","FACET","FAINT",
  "FAITH","FALSE","FANCY","FATAL","FAULT","FEAST","FENCE","FEWER",
  "FIBER","FIELD","FIFTH","FIGHT","FINAL","FLAME","FLASH","FLESH",
  "FLINT","FLOAT","FLOCK","FLOOD","FLOOR","FLOUR","FLUID","FLUTE",
  "FOCAL","FORCE","FORGE","FORTH","FORUM","FOUND","FRAME","FRANK",
  "FRESH","FRONT","FROST","FRUIT","FULLY","FUNNY","GHOST","GIANT",
  "GIVEN","GLASS","GLEAM","GLOBE","GLOOM","GLORY","GLOVE","GOING",
  "GRACE","GRADE","GRAIN","GRAND","GRANT","GRAPH","GRASP","GRASS",
  "GRAVE","GREAT","GREEN","GREET","GRIEF","GRIND","GROAN","GROSS",
  "GROUP","GROVE","GROWN","GUARD","GUESS","GUIDE","GUILD","GUILT",
  "HABIT","HAPPY","HARSH","HAVEN","HEART","HEAVY","HENCE","HONEY",
  "HONOR","HORSE","HOTEL","HOUSE","HUMAN","HUMOR","IDEAL","IMAGE",
  "IMPLY","INCUR","INDEX","INNER","INPUT","ISSUE","IVORY","JOINT",
  "JUDGE","JUICE","KEEPS","KNIFE","KNOCK","KNOWN","LABEL","LABOR",
  "LANCE","LARGE","LASER","LATER","LAUGH","LAYER","LEARN","LEASE",
  "LEGAL","LEVEL","LIGHT","LIMIT","LINEN","LIVER","LODGE","LOGIC",
  "LOOSE","LOVER","LOWER","LOYAL","LUNAR","LUNCH","MAGIC","MAJOR",
  "MAKER","MANOR","MARCH","MATCH","MAYOR","MEDIA","MERCY","MERGE",
  "METAL","MICRO","MIGHT","MINOR","MIXED","MODEL","MONEY","MONTH",
  "MORAL","MOUNT","MOUTH","MOVED","MOVIE","MUSIC","NAKED","NERVE",
  "NEVER","NIGHT","NOBLE","NOISE","NORTH","NOTED","NOVEL","NURSE",
  "OCEAN","OFFER","OLIVE","ONSET","OPERA","ORDER","OTHER","OUGHT",
  "OUTER","OWNED","OWNER","OXIDE","PAINT","PANEL","PASTE","PATCH",
  "PAUSE","PEACE","PEARL","PENNY","PHASE","PHONE","PHOTO","PIANO",
  "PIECE","PITCH","PIVOT","PIXEL","PLACE","PLAIN","PLANE","PLANT",
  "PLATE","PLAZA","PLEAD","PLUMB","PLUME","POINT","POLAR","POUND",
  "POWER","PRESS","PRIDE","PRIME","PRINT","PRIOR","PRIZE","PROOF",
  "PROUD","PROVE","PSALM","PURSE","QUEEN","QUEST","QUICK","QUIET",
  "QUITE","QUOTE","RADAR","RADIO","RAISE","RANGE","RAPID","RATIO",
  "REACH","REACT","READY","REALM","REBEL","REFER","REIGN","RELAX",
  "RENEW","REPLY","RIDER","RIFLE","RIGHT","RIGID","RISKY","RIVAL",
  "RIVER","ROBIN","ROBOT","ROCKY","ROMAN","ROOTS","ROUGH","ROUND",
  "ROUTE","ROYAL","RULER","RURAL","SAINT","SALAD","SAUCE","SCALE",
  "SCARE","SCENE","SCENT","SCOPE","SCOUT","SENSE","SERVE","SEVEN",
  "SHADE","SHAFT","SHAKE","SHALL","SHAME","SHAPE","SHARE","SHARK",
  "SHARP","SHEER","SHELF","SHELL","SHIFT","SHINE","SHOCK","SHORE",
  "SHORT","SHOUT","SIGHT","SINCE","SIXTH","SIXTY","SKILL","SKULL",
  "SLASH","SLATE","SLEEP","SLICE","SLIDE","SLOPE","SMART","SMELL",
  "SMILE","SMOKE","SNAKE","SOLAR","SOLID","SOLVE","SORRY","SOUND",
  "SOUTH","SPACE","SPARE","SPEAK","SPEED","SPEND","SPILL","SPINE",
  "SPOKE","SPORT","SPRAY","SQUAD","STACK","STAFF","STAGE","STAIN",
  "STAKE","STALE","STAND","STARK","START","STATE","STEAL","STEAM",
  "STEEL","STEEP","STEER","STERN","STICK","STILL","STOCK","STONE",
  "STOOD","STORE","STORM","STORY","STOVE","STRIP","STUCK","STUFF",
  "STYLE","SUGAR","SUITE","SUPER","SURGE","SWAMP","SWEAR","SWEEP",
  "SWEET","SWIFT","SWING","SWORD","TABLE","TAKEN","TASTE","TEACH",
  "TEETH","THEME","THICK","THING","THINK","THIRD","THORN","THOSE",
  "THREE","THROW","THUMB","TIGER","TIGHT","TIMER","TIRED","TITLE",
  "TOKEN","TOTAL","TOUCH","TOUGH","TOWER","TOXIC","TRACE","TRACK",
  "TRADE","TRAIL","TRAIN","TRAIT","TRASH","TREAT","TREND","TRIAL",
  "TRIBE","TRICK","TRIED","TROOP","TRUCK","TRULY","TRUST","TRUTH",
  "TUMOR","TWICE","TWIST","ULTRA","UNDER","UNION","UNITE","UNITY",
  "UNTIL","UPPER","URBAN","USAGE","USUAL","VALID","VALUE","VALVE",
  "VENUE","VERSE","VIDEO","VIGOR","VIRAL","VIRUS","VISIT","VITAL",
  "VIVID","VOCAL","VOICE","VOTED","VOTER","WAGON","WASTE","WATCH",
  "WATER","WEAVE","WHEEL","WHERE","WHICH","WHITE","WHOLE","WIDER",
  "WITCH","WOMEN","WORLD","WORSE","WORST","WORTH","WOULD","WOUND",
  "WRITE","WRONG","WROTE","YACHT","YIELD","YOUNG","YOUTH","ABOARD",
  "ABSORB","ACCENT","ACCESS","ACCORD","ACCUSE","ADJUST","ADMIRE",
  "AFFAIR","AFFIRM","AGENDA","AGENTS","AGREES","ALLIED","ALMOST",
  "AMUSED","ANCHOR","ANYWAY","AROUND","ARREST","ARRIVE","ASPECT",
  "ASSIGN","ASSURE","AUTUMN","AVENUE","BARELY","BARREL","BASKET",
  "BATTLE","BEAUTY","BECOME","BEFORE","BEHALF","BEHAVE","BEHIND",
  "BELONG","BESIDE","BETTER","BEYOND","BISHOP","BLANKET","BOTTLE",
  "BOUNCE","BRANCH","BREATH","BREEZE","BRIDGE","BRIGHT","BROKEN",
  "BRONZE","BRUTAL","BUBBLE","BUCKET","BUDGET","BULLET","BUNDLE",
  "BURDEN","BUREAU","BUTTON","CANDLE","CARBON","CAREER","CASTLE",
  "CASUAL","CAUGHT","CEMENT","CENTRE","CHANCE","CHANGE","CHAPEL",
  "CHARGE","CHOSEN","CHROME","CHURCH","CIRCLE","CLEVER","CLIMAX",
  "CLOSET","COARSE","COFFEE","COLUMN","COMBAT","COMEDY","COMMIT",
  "COMMON","COMPLY","CONVEY","COOKIE","COPPER","CORNER","COSTLY",
  "COTTON","COUPLE","COUSIN","CRADLE","CREATE","CREDIT","CRISIS",
  "CRUISE","CUSTOM","DAMAGE","DANGER","DEALER","DEBATE","DECADE",
  "DECIDE","DEFEAT","DEFEND","DEFINE","DEGREE","DEMAND","DEPEND",
  "DEPLOY","DESERT","DESIGN","DESIRE","DETAIL","DETECT","DEVICE",
  "DEVOTE","DIFFER","DINING","DINNER","DIRECT","DIVIDE","DOMAIN",
  "DONATE","DOUBLE","DRIVER","EASILY","EATING","EFFECT","EFFORT",
  "EIGHTH","ELEVEN","EMERGE","EMPIRE","EMPLOY","ENABLE","ENDING",
  "ENERGY","ENGAGE","ENGINE","ENOUGH","ENSURE","ENTIRE","ENTITY",
  "EQUITY","ESCAPE","ESTATE","ETHNIC","EVOLVE","EXCEED","EXCUSE",
  "EXEMPT","EXPAND","EXPECT","EXPERT","EXPORT","EXTEND","EXTENT",
  "FABRIC","FAIRLY","FALLEN","FARMER","FASTER","FATHER","FELLOW",
  "FIGURE","FILTER","FINGER","FINISH","FLIGHT","FLOWER","FLYING",
  "FOLLOW","FORBID","FOREST","FORGET","FORMAL","FORMAT","FORMER",
  "FOSTER","FREELY","FREEZE","FROZEN","FULFIL","GARDEN","GATHER",
  "GENDER","GENTLE","GIFTED","GLOBAL","GOLDEN","GOVERN","GROWTH",
  "GUILTY","GUITAR","HANDLE","HAPPEN","HARBOR","HARDLY","HEADED",
  "HEALTH","HEAVEN","HEIGHT","HIDDEN","HIGHLY","HONEST","HORROR",
  "IMPOSE","IMPORT","INCOME","INDEED","INDOOR","INFANT","INFORM",
  "INJURE","INLAND","INSECT","INSIDE","INSIST","INTACT","INTEND",
  "INVEST","ISLAND","ITSELF","JACKET","JUNGLE","JUNIOR","KIDNEY",
  "KINDLY","KNIGHT","LADDER","LAUNCH","LAWYER","LAYOUT","LEADER",
  "LEAGUE","LESSEN","LETTER","LIKELY","LINING","LISTEN","LITTLE",
  "LIVING","LOCATE","LONELY","LOSSES","LOVELY","MAINLY","MANAGE",
  "MANNER","MARBLE","MARGIN","MARINE","MARKER","MARKET","MASTER",
  "MATTER","MEDIUM","MEMBER","MENTAL","MERELY","MIDDLE","MIGHTY",
  "MILDLY","MINING","MINUTE","MIRROR","MOBILE","MODERN","MODEST",
  "MOMENT","MONKEY","MOSTLY","MOTHER","MOTION","MOTIVE","MURDER",
  "MUSEUM","MUTUAL","NAMELY","NARROW","NATION","NATIVE","NATURE",
  "NEARBY","NEARLY","NEATLY","NOTICE","NUMBER","OBJECT","OBTAIN",
  "OCCUPY","OFFEND","OFFICE","OPPOSE","OUTPUT","PACKED","PALACE",
  "PARADE","PARENT","PARTLY","PATENT","PATROL","PERMIT","PHRASE",
  "PILLOW","PLANET","PLAYER","PLEDGE","PLENTY","PLUNGE","POCKET",
  "POETRY","POISON","POLICE","POLICY","POLITE","POORLY","POTATO",
  "PRAISE","PRAYER","PREFER","PRETTY","PRINCE","PRISON","PROFIT",
  "PROMPT","PROPER","PROVEN","PUBLIC","PURSUE","PUZZLE","RANDOM",
  "RANGER","RARELY","RATHER","RATING","READER","REASON","RECALL",
  "RECENT","RECORD","REDUCE","REFORM","REFUSE","REGARD","REGIME",
  "REGION","REGRET","REJECT","RELATE","RELIEF","REMAIN","REMEDY",
  "REMOTE","REMOVE","RENDER","REPAIR","REPEAT","REPORT","RESCUE",
  "RESIGN","RESIST","RESORT","RESULT","RETAIL","RETAIN","RETIRE",
  "RETURN","REVEAL","REVIEW","REVOLT","REWARD","RULING","RUNNER",
  "SAFELY","SAILOR","SALARY","SALMON","SAMPLE","SCHEME","SCHOOL",
  "SCREEN","SCRIPT","SEARCH","SECRET","SECTOR","SECURE","SEEING",
  "SELDOM","SELECT","SELLER","SENIOR","SERIES","SERVER","SETTLE",
  "SEVERE","SHADOW","SHIELD","SHOWER","SIGNAL","SILENT","SILVER",
  "SIMPLE","SINGLE","SISTER","SKETCH","SLIGHT","SMOOTH","SOCCER",
  "SOLEMN","SOONER","SOURCE","SPOKEN","SPREAD","SPRING","STABLE",
  "STANCE","STATUS","STEADY","STEREO","STITCH","STOLEN","STRAIN",
  "STRAND","STREAM","STREET","STRICT","STRIKE","STRING","STROKE",
  "STRONG","STRUCK","STUDIO","SUBMIT","SUDDEN","SUFFER","SUMMIT",
  "SUNDAY","SUPERB","SUPPLY","SURELY","SURVEY","SWITCH","SYMBOL",
  "TACKLE","TALENT","TARGET","TEMPLE","TENDER","TENURE","THANKS",
  "THIRTY","THREAT","THRILL","THRONE","TIMBER","TISSUE","TONGUE",
  "TOWARD","TRAVEL","TREATY","TRIPLE","TROOPS","TUNNEL","TWELVE",
  "UNFAIR","UNIQUE","UNLESS","UNLIKE","UNVEIL","UPWARD","USEFUL",
  "VALLEY","VANISH","VESSEL","VICTIM","VIEWER","VIRGIN","VISION",
  "VOLUME","WALKER","WARMLY","WEALTH","WEEKLY","WHOLLY","WIDELY",
  "WINDOW","WINNER","WINTER","WISDOM","WONDER","WORKER","WORTHY",
  "WRITER","YELLOW"
)

mots_en <- unique(mots_en[nchar(mots_en) >= 3 & nchar(mots_en) <= 10])



# MYSTERY WORDS ----

# French mystery words (definitions kept in French) 

mysteres_fr <- list(
  list(mot="CHAMPION", definition="Celui qui remporte la victoire finale"),
  list(mot="VICTOIRE", definition="Le resultat souhaite de tout athlete"),
  list(mot="PENALTY",  definition="Tir accorde apres une faute dans la surface"),
  list(mot="ATHLETE",  definition="Personne qui pratique un sport intensement"),
  list(mot="PODIUM",   definition="Estrade ou montent les trois premiers"),
  list(mot="RECORD",   definition="Performance jamais atteinte auparavant"),
  list(mot="GARDIEN",  definition="Celui qui protege les buts de son equipe"),
  list(mot="BRONZE",   definition="Medaille du troisieme de la competition"),
  list(mot="FINALE",   definition="Dernier match pour decrocher le titre"),
  list(mot="SUPPORTER",definition="Personne qui encourage son equipe avec passion"),
  list(mot="CLASSEMENT",definition="Tableau qui ordonne les equipes par resultats"),
  list(mot="TECHNIQUE",definition="Maitrise des gestes propres a un sport"),
  list(mot="STRATEGIE",definition="Plan elabore pour remporter la victoire"),
  list(mot="MARATHON", definition="Course de fond de plus de 42 kilometres"),
  list(mot="ARBITRE",  definition="Personne qui fait respecter les regles du jeu"),
  list(mot="MEDAILLE", definition="Recompense en metal pour les meilleurs athletes"),
  list(mot="COURAGE",  definition="Force morale face a la difficulte"),
  list(mot="EXPLOIT",  definition="Action remarquable et extraordinaire"),
  list(mot="TROPHEE",  definition="Objet symbolique remis au vainqueur"),
  list(mot="SAISON",   definition="Periode de matchs du calendrier sportif"),
  list(mot="RIVALE",   definition="Equipe ou personne que l on affronte"),
  list(mot="ADRESSE",  definition="Habilete et precision dans le geste"),
  list(mot="COMBAT",   definition="Affrontement entre deux adversaires"),
  list(mot="DEFAITE",  definition="Le contraire de la victoire"),
  list(mot="ESPRIT",   definition="Mentalite et etat psychologique du sportif"),
  list(mot="FAVORI",   definition="Celui que tout le monde donne gagnant"),
  list(mot="HONNEUR",  definition="Fierte et dignite dans la competition"),
  list(mot="INTENSE",  definition="Effort physique pousse au maximum"),
  list(mot="JUSTICE",  definition="Equite dans l application des regles"),
  list(mot="MENTAL",   definition="Force psychologique du competiteur"),
  list(mot="OBJECTIF", definition="But que le sportif cherche a atteindre"),
  list(mot="PARCOURS", definition="Chemin et etapes d une carriere sportive"),
  list(mot="QUALITE",  definition="Ce qui distingue un grand joueur"),
  list(mot="REVANCHE", definition="Match retour pour effacer une defaite"),
  list(mot="SUSPENSE", definition="Tension quand le resultat est incertain"),
  list(mot="TALENT",   definition="Don naturel pour exceller dans un sport"),
  list(mot="TRIOMPHE", definition="Grande victoire eclatante et memorable"),
  list(mot="ENDURANCE",definition="Capacite a maintenir un effort prolonge"),
  list(mot="FAIRPLAY", definition="Respect des regles et de l adversaire"),
  list(mot="EQUIPE",   definition="Groupe de joueurs unis vers un meme but"),
  list(mot="BUTEUR",   definition="Joueur specialise dans l art de marquer"),
  list(mot="CORNER",   definition="Coup de pied tire depuis le coin du terrain"),
  list(mot="SPRINT",   definition="Course a vitesse maximale sur courte distance"),
  list(mot="STADE",    definition="Enceinte ou se deroulent les matchs")
)



# English mystery words

mysteres_en <- list(
  list(mot="CHAMPION", definition="The winner of a major competition"),
  list(mot="VICTORY",  definition="The ultimate goal of every contest"),
  list(mot="ATHLETE",  definition="A person who competes in sports"),
  list(mot="STADIUM",  definition="A large venue for sporting events"),
  list(mot="PLAYOFF",  definition="A series of games to determine the winner"),
  list(mot="TROPHY",   definition="A prize awarded to the winner"),
  list(mot="UNDERDOG", definition="A competitor not expected to win"),
  list(mot="TRAINING", definition="The process of preparing for competition"),
  list(mot="DEFENDER", definition="A player who protects their team goal"),
  list(mot="CAPTAIN",  definition="The leader of a sports team"),
  list(mot="REFEREE",  definition="The person who enforces the rules"),
  list(mot="COURAGE",  definition="Bravery in the face of difficulty"),
  list(mot="EXPLOIT",  definition="A remarkable and daring achievement"),
  list(mot="SEASON",   definition="A period of scheduled games"),
  list(mot="RIVALS",   definition="Teams or people who compete against each other"),
  list(mot="SPIRIT",   definition="The attitude and energy of a competitor"),
  list(mot="TALENT",   definition="Natural ability to excel in a sport"),
  list(mot="DEFEAT",   definition="The opposite of winning"),
  list(mot="RECORD",   definition="The best ever performance in a discipline"),
  list(mot="LEGEND",   definition="A player whose fame lasts forever"),
  list(mot="FINALS",   definition="The last and most important matches"),
  list(mot="TACKLE",   definition="A move to take the ball from an opponent"),
  list(mot="SPRINT",   definition="A short burst of maximum speed running"),
  list(mot="BRONZE",   definition="The medal for third place"),
  list(mot="GOALIE",   definition="The player guarding the net"),
  list(mot="LEAGUE",   definition="An organized group of teams that compete"),
  list(mot="SCORE",    definition="The number of points in a match"),
  list(mot="MENTAL",   definition="The psychological strength of an athlete"),
  list(mot="GLORY",    definition="Great honor achieved through competition"),
  list(mot="BENCH",    definition="Where substitute players wait their turn"),
  list(mot="CROWD",    definition="The mass of spectators watching a game"),
  list(mot="COACH",    definition="The person who trains and guides the team"),
  list(mot="PITCH",    definition="The field where a match is played"),
  list(mot="MEDAL",    definition="A metal disc given to top finishers"),
  list(mot="FINAL",    definition="The last match to decide the winner"),
  list(mot="NERVE",    definition="What you need to stay calm under pressure"),
  list(mot="SKILL",    definition="The ability developed through practice"),
  list(mot="MATCH",    definition="A contest between two sides"),
  list(mot="POWER",    definition="Physical strength and force"),
  list(mot="PRIDE",    definition="Deep satisfaction from achievement")
)





# Grid generation algorithm ----


#' Get allowed directions based on difficulty level
#' @param difficulty "easy", "medium" or "difficult"
#' @return List of direction vectors c(dr, dc)


get_directions <- function(difficulty) {
  switch(difficulty,
         easy    = list(c(0,1), c(1,0)),
         medium     = list(c(0,1), c(1,0), c(1,1), c(1,-1)),
         difficult = list(c(0,1), c(0,-1), c(1,0), c(-1,0),
                          c(1,1), c(1,-1), c(-1,1), c(-1,-1))
  )
}



#' Attempt to place a word on the grid
#' @param grid Character matrix (NA = empty cell)
#' @param word Word to place (string)
#' @param size Grid size (number of rows/columns)
#' @param directions List of allowed directions
#' @return list(grid, positions) or NULL if placement failed


place_word <- function(grid, word, size, directions) {
  letters <- strsplit(word, "")[[1]]
  wlen <- length(letters)
  dirs_shuffled <- sample(directions)
  
  for (attempt in seq_len(200)) {
    dir <- dirs_shuffled[[(attempt - 1) %% length(dirs_shuffled) + 1]]
    dr <- dir[1]; dc <- dir[2]
    
    # Compute valid starting position bounds
    min_r <- ifelse(dr >= 0, 1, wlen)
    max_r <- ifelse(dr <= 0, size, size - wlen + 1)
    min_c <- ifelse(dc >= 0, 1, wlen)
    max_c <- ifelse(dc <= 0, size, size - wlen + 1)
    
    if (min_r > max_r || min_c > max_c) next
    
    r0 <- sample(min_r:max_r, 1)
    c0 <- sample(min_c:max_c, 1)
    
    # Check if word fits without conflicts
    positions <- matrix(nrow = wlen, ncol = 2)
    ok <- TRUE
    for (i in seq_len(wlen)) {
      nr <- r0 + dr * (i - 1)
      nc <- c0 + dc * (i - 1)
      if (nr < 1 || nr > size || nc < 1 || nc > size) { ok <- FALSE; break }
      if (!is.na(grid[nr, nc]) && grid[nr, nc] != letters[i]) { ok <- FALSE; break }
      positions[i, ] <- c(nr, nc)
    }
    
    if (ok) {
      for (i in seq_len(wlen)) {
        grid[positions[i, 1], positions[i, 2]] <- letters[i]
      }
      return(list(grid = grid, positions = positions))
    }
  }
  return(NULL)
}


#' Generate a complete word search grid
#' @param size Grid size (10, 12 or 15)
#' @param words Character vector of available words
#' @param mystery_word The mystery word to hide in remaining cells
#' @param difficulty "easy", "medium" or "difficult"
#' @return List with grid, placed, wpos, myst, mpos, size (or NULL on failure)


generate_grid <- function(size, words, mystery_word, difficulty) {
  directions <- get_directions(difficulty)
  total <- size * size
  myst_len <- nchar(mystery_word)
  target_occ <- total - myst_len
  eligible <- words[nchar(words) <= size]
  
  for (attempt in seq_len(100)) {
    grid <- matrix(NA_character_, nrow = size, ncol = size)
    placed <- character(0)
    wpos <- list()
    cell_count <- matrix(0L, nrow = size, ncol = size)
    occ <- 0L
    
    # Balanced shuffle: interleave short / medium / long words
    short_w <- sample(eligible[nchar(eligible) <= 5])
    med_w   <- sample(eligible[nchar(eligible) %in% 6:7])
    long_w  <- sample(eligible[nchar(eligible) >= 8])
    max_n   <- max(length(short_w), length(med_w), length(long_w))
    cands   <- character(0)
    for (ii in seq_len(max_n)) {
      if (ii <= length(med_w))   cands <- c(cands, med_w[ii])
      if (ii <= length(short_w)) cands <- c(cands, short_w[ii])
      if (ii <= length(long_w))  cands <- c(cands, long_w[ii])
    }
    
    for (w in cands) {
      if (occ >= target_occ) break
      
      backup <- grid
      result <- place_word(grid, w, size, directions)
      if (is.null(result)) next
      
      grid <- result$grid
      pos  <- result$positions
      
      # Count newly occupied cells + check at least 1 unique cell
      new_occ <- 0L; has_unique <- FALSE
      for (i in seq_len(nrow(pos))) {
        if (cell_count[pos[i,1], pos[i,2]] == 0L) {
          new_occ <- new_occ + 1L
          has_unique <- TRUE
        }
      }
      
      empty_after <- total - (occ + new_occ)
      # Reject if too few empty cells remain or word has no unique letter
      if (empty_after < myst_len || !has_unique) { grid <- backup; next }
      
      # Accept this word
      placed <- c(placed, w)
      wpos[[w]] <- pos
      for (i in seq_len(nrow(pos))) {
        r <- pos[i,1]; cc <- pos[i,2]
        if (cell_count[r, cc] == 0L) occ <- occ + 1L
        cell_count[r, cc] <- cell_count[r, cc] + 1L
      }
      
      # Perfect fit: exactly myst_len empty cells remaining
      if (empty_after == myst_len) break
    }
    
    # Validate result
    empty_cells <- which(is.na(grid), arr.ind = TRUE)
    if (nrow(empty_cells) == myst_len && length(placed) >= 5) {
      # Sort empty cells in reading order (top-left to bottom-right)
      empty_cells <- empty_cells[order(empty_cells[,1], empty_cells[,2]), ]
      myst_letters <- strsplit(mystery_word, "")[[1]]
      myst_pos <- matrix(nrow = myst_len, ncol = 2)
      for (i in seq_len(myst_len)) {
        grid[empty_cells[i,1], empty_cells[i,2]] <- myst_letters[i]
        myst_pos[i, ] <- empty_cells[i, ]
      }
      cat(sprintf("  Grid %dx%d [%s]: %d words placed, %d attempt(s)\n",
                  size, size, difficulty, length(placed), attempt))
      return(list(grid=grid, placed=placed, wpos=wpos,
                  myst=mystery_word, mpos=myst_pos, size=size))
    }
  }
  warning(sprintf("Failed to generate grid %dx%d %s after 100 attempts",
                  size, size, difficulty))
  return(NULL)
}



# Pre-generate default grid ----


set.seed(NULL)

# Pre-generate one grid for the default config (12x12, medium, FR)
myst_default <- mysteres_fr[[sample(length(mysteres_fr), 1)]]
grille_default <- generate_grid(12, mots_fr, myst_default$mot, "medium")

# Print statistics
cat(sprintf("\nDefault grid: %dx%d, %d words placed, mystery word = %s\n",
            grille_default$size, grille_default$size,
            length(grille_default$placed), grille_default$myst))

# Direction distribution
if (length(grille_default$placed) > 0) {
  dir_names <- sapply(grille_default$placed, function(w) {
    pos <- grille_default$wpos[[w]]
    if (nrow(pos) < 2) return("?")
    dr <- sign(pos[2,1] - pos[1,1])
    dc <- sign(pos[2,2] - pos[1,2])
    paste0("(", dr, ",", dc, ")")
  })
  cat("Directions used:\n")
  print(table(dir_names))
}


# BUILD JSON ----

# Same word pool for all difficulty levels (difficulty only affects directions)

dict_data <- list(
  fr = list(
    easy    = list(mots = mots_fr, mysteres = mysteres_fr),
    medium     = list(mots = mots_fr, mysteres = mysteres_fr),
    difficult = list(mots = mots_fr, mysteres = mysteres_fr)
  ),
  en = list(
    easy    = list(mots = mots_en, mysteres = mysteres_en),
    medium     = list(mots = mots_en, mysteres = mysteres_en),
    difficult = list(mots = mots_en, mysteres = mysteres_en)
  )
)

dict_json <- toJSON(dict_data, auto_unbox = TRUE)


# Inject into template and write output ----

template_path <- here("template.html")
if (!file.exists(template_path)) {
  stop("template.html not found in: ", here(),
       "\nMake sure template.html is in the same directory as this script.")
}

template <- readLines(template_path, encoding = "UTF-8", warn = FALSE)
template <- paste(template, collapse = "\n")

# Replace placeholder with JSON data from R
html_final <- sub("%%DICT_JSON%%", dict_json, template, fixed = TRUE)

# Write final HTML file
output_file <- here("output","mots_meles.html")
writeLines(html_final, output_file, useBytes = TRUE)
