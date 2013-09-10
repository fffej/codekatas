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
            return m_MineField[0] == '*' ? "*" : "0";
        }
    }
}
