let currentPage = 0;
const answers = [];
const questions = [
    {
        group: "Physical Health",
        questions: [
            "Do you experience pain or discomfort in your hands or wrists after playing?",
            "Do you experience neck or back pain after playing for extended periods?",
            "Do you feel tension or fatigue in your body after long practice sessions?",
            "Do you take regular breaks during practice sessions to stretch or rest?",
            "Do you experience any hearing issues or discomfort from loud environments?"
        ]
    },
    {
        group: "Mental Health",
        questions: [
            "Do you often feel stressed or anxious before performances?",
            "Do you struggle with self-doubt or imposter syndrome?",
            "Do you experience burnout after long rehearsals?",
            "Do you find it difficult to relax after practice?",
            "Do you have trouble sleeping due to stress or overthinking?",
            "Do you feel emotionally drained after performances?"
        ]
    },
    {
        group: "Overall Well-being",
        questions: [
            "Do you maintain a healthy diet and stay hydrated?",
            "Do you incorporate physical exercise into your routine?",
            "Do you have a support network for mental health?",
            "Do you feel balanced between music and personal time?",
            "Do you seek professional help when needed?"
        ]
    }
];

function startQuiz() {
    document.getElementById('start-page').style.display = 'none';
    document.getElementById('questionnaire').style.display = 'block';
    showQuestions();
}

function showQuestions() {
    const currentGroup = questions[currentPage];
    const form = document.getElementById('health-form');
    form.innerHTML = `<h2>${currentGroup.group}</h2>`;

    currentGroup.questions.forEach((q, index) => {
        form.innerHTML += `
            <div class="question">
                <label>${q}</label><br>
                <input type="radio" name="q${index + 1}" value="yes"> Yes
                <input type="radio" name="q${index + 1}" value="no"> No
            </div>
        `;
    });

    document.getElementById('prev-button').style.display = currentPage > 0 ? 'inline-block' : 'none';
    document.getElementById('next-button').innerText = currentPage === questions.length - 1 ? 'Submit' : 'Next';
}

function changeQuestion(direction) {
    const form = document.getElementById('health-form');
    const currentGroup = questions[currentPage];

    currentGroup.questions.forEach((q, index) => {
        const answer = document.querySelector(`input[name="q${index + 1}"]:checked`);
        answers.push(answer ? answer.value : 'no');
    });

    currentPage += direction;

    if (currentPage < questions.length) {
        showQuestions();
    } else {
        document.getElementById('questionnaire').style.display = 'none';
        showTipsPage();
    }
}

function showTipsPage() {
    document.getElementById('tips-dashboard').style.display = 'block';
}

function showCategoryTips(category) {
    const tipsContainer = document.getElementById('tips-container');
    tipsContainer.innerHTML = '';

    let tips;

    if (category === 'Physical Health') {
        tips = `
            <div class="fade-in">
                <h3>Physical Health Tips</h3>
                <ul>
                    <li>Take breaks every 30-45 minutes to stretch and relax your muscles.</li>
                    <li>Maintain good posture to avoid strain on your neck, back, and wrists.</li>
                    <li>Stay hydrated and eat a balanced diet to support physical health.</li>
                </ul>
            </div>
        `;
    } else if (category === 'Mental Health') {
        tips = `
            <div class="fade-in">
                <h3>Mental Health Tips</h3>
                <ul>
                    <li>Practice mindfulness or meditation to reduce stress before performances.</li>
                    <li>Set realistic goals to avoid burnout and maintain motivation.</li>
                    <li>Seek support from peers or professionals if you're feeling overwhelmed.</li>
                </ul>
            </div>
        `;
    } else {
        tips = `
            <div class="fade-in">
                <h3>Overall Well-being Tips</h3>
                <ul>
                    <li>Ensure a balance between music and personal time to avoid burnout.</li>
                    <li>Stay active and exercise regularly to support both physical and mental health.</li>
                    <li>Seek help if you experience mental or physical health challenges.</li>
                </ul>
            </div>
        `;
    }

    tipsContainer.innerHTML = tips;
}
