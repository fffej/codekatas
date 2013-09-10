using NUnit.Framework;
using FluentAssertions;

namespace Fatvat.Katas.MineSweeper
{
    [TestFixture]
    public class MineSweeperTest
    {
        [Test]
        public void EmptyMineField()
        {
            var mineField = new MineField(".");
            Assert.That(mineField.Show(), Is.EqualTo("0"));
        }

        [Test]
        public void OneMine()
        {
            var mineField = new MineField("*");
            Assert.That(mineField.Show(), Is.EqualTo("*"));
        }

        [Test]
        public void OneLine()
        {
            var mineField = new MineField(".*.");
            Assert.That(mineField.Show(), Is.EqualTo("1*1"));
        }
    }

    public class MineField
    {
        private readonly string m_MineField;

        public MineField(string mineField)
        {
            m_MineField = mineField;
        }

        public string Show()
        {
            var result = "";
            for (var i = 0; i < m_MineField.Length; ++i)
            {
                var surroundingMines = 0;
                if (i > 0 && m_MineField[i - 1] == '*')
                {
                    surroundingMines++;
                }
                if (i + 1 < m_MineField.Length && m_MineField[i + 1] == '*')
                {
                    surroundingMines++;
                }

                if (m_MineField[i] == '.')
                {
                    result += surroundingMines;
                }
                else
                {
                    result += '*';
                }
            }

            return result;
        }
    }
}
